--
-- PIC16F84 compatible microcontroller core
--
-- Version : 0220
--
-- Copyright (c) 2001-2002 Daniel Wallner (jesus@opencores.org)
--
-- All rights reserved
--
-- Redistribution and use in source and synthezised forms, with or without
-- modification, are permitted provided that the following conditions are met:
--
-- Redistributions of source code must retain the above copyright notice,
-- this list of conditions and the following disclaimer.
--
-- Redistributions in synthesized form must reproduce the above copyright
-- notice, this list of conditions and the following disclaimer in the
-- documentation and/or other materials provided with the distribution.
--
-- Neither the name of the author nor the names of other contributors may
-- be used to endorse or promote products derived from this software without
-- specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
-- THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
-- PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE
-- LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
-- CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
-- SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
-- INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
-- CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
-- ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.
--
-- Please report bugs to the author, but before you do so, please
-- make sure that this is not a derivative work and that
-- you have the latest version of this file.
--
-- The latest version of this file can be found at:
--	http://www.opencores.org/cvsweb.shtml/t51/
--
-- Limitations :
--	No port B pullup
--	No EEPROM
--
-- File history :
--

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use work.PPX_Pack.all;

entity P16F84 is
	generic(
		SyncReset : boolean := true);
	port(
		Clk		: in std_logic;
		Reset_n	: in std_logic;
		T0CKI	: in std_logic;
		INT		: in std_logic;
		Port_A	: inout std_logic_vector(7 downto 0);
		Port_B	: inout std_logic_vector(7 downto 0)
	);
end P16F84;

architecture rtl of P16F84 is

	constant	InstructionLength	: integer := 14;
	constant	ROMAddressWidth		: integer := 10;
	constant	StackAddrWidth		: integer := 3;
	constant	TopBoot				: boolean := false;

	component ROM84
		port(
			Clk	: in std_logic;
			A	: in std_logic_vector(9 downto 0);
			D	: out std_logic_vector(13 downto 0)
		);
	end component;

	signal	Reset_s_n	: std_logic;
	signal	ROM_Addr	: std_logic_vector(9 downto 0);
	signal	ROM_Data	: std_logic_vector(InstructionLength - 1 downto 0);
	signal	Instruction	: std_logic_vector(InstructionLength - 1 downto 0);
	signal	File_Addr	: std_logic_vector(InstructionLength - 6 downto 0);
	signal	File_Addr_r	: std_logic_vector(InstructionLength - 6 downto 0);
	signal	File_CS		: std_logic_vector(6 downto 5);
	signal	TMR_CS		: std_logic;
	signal	RAM_CS		: std_logic;
	signal	File_Rd		: std_logic;
	signal	File_Wr		: std_logic;
	signal	Tris_A_Rd	: std_logic;
	signal	Tris_A_Wr	: std_logic;
	signal	Tris_B_Rd	: std_logic;
	signal	Tris_B_Wr	: std_logic;
	signal	RAM_Data	: std_logic_vector(7 downto 0);
	signal	Op_Bus		: std_logic_vector(7 downto 0);
	signal	Res_Bus		: std_logic_vector(7 downto 0);
	signal	OPTION		: std_logic_vector(7 downto 0);
	signal	INTCON		: std_logic_vector(7 downto 0);
	signal	Int_Trig	: std_logic;
	signal	Int_Acc		: std_logic;
	signal	Int_Ret		: std_logic;
	signal	TOF			: std_logic;
	signal	Old_B		: std_logic_vector(7 downto 4);
	signal	Old_INT		: std_logic;

begin

	-- Synchronise reset
	process (Reset_n, Clk)
		variable Reset_v : std_logic;
	begin
		if Reset_n = '0' then
			if SyncReset then
				Reset_s_n <= '0';
				Reset_v := '0';
			end if;
		elsif Clk'event and Clk = '1' then
			if SyncReset then
				Reset_s_n <= Reset_v;
				Reset_v := '1';
			end if;
		end if;
	end process;

	g_reset : if not SyncReset generate
		Reset_s_n <= Reset_n;
	end generate;

	-- Address decoder
	File_CS(5) <= '1' when to_integer(unsigned(File_Addr_r(7 downto 0))) = 5 else '0';
	File_CS(6) <= '1' when to_integer(unsigned(File_Addr_r(7 downto 0))) = 6 else '0';
	Tris_A_Rd <= '1' when to_integer(unsigned(File_Addr_r(7 downto 0))) = 133 and File_Rd = '1' else '0';
	Tris_B_Rd <= '1' when to_integer(unsigned(File_Addr_r(7 downto 0))) = 134 and File_Rd = '1' else '0';
	Tris_A_Wr <= '1' when (to_integer(unsigned(File_Addr_r(7 downto 0))) = 133 and File_Wr = '1') or
				Instruction(13 downto 0) = "00000001100101" else '0';
	Tris_B_Wr <= '1' when (to_integer(unsigned(File_Addr_r(7 downto 0))) = 134 and File_Wr = '1') or
				Instruction(13 downto 0) = "00000001100110" else '0';
	RAM_CS <= '1' when File_Addr_r(6 downto 4) /= "000" or File_Addr_r(3 downto 2) = "11" else '0';
	TMR_CS <= '1' when to_integer(unsigned(File_Addr_r(7 downto 0))) = 1 else '0';

	-- Register File
	Op_Bus <= RAM_Data when RAM_CS = '1' and File_Rd = '1' else "ZZZZZZZZ";
	pr : PPX_RAM
		generic map(Bottom => 12, Top => 79, AddrWidth => 7)
		port map(
			Clk => Clk,
			CS => RAM_CS,
			Wr => File_Wr,
			Addr => File_Addr(6 downto 0),
			Data_In => Res_Bus,
			Data_Out => RAM_Data);

	-- Option Register
	Op_Bus <= OPTION when
			to_integer(unsigned(File_Addr_r(7 downto 0))) = 129 and
			File_Rd = '1' else "ZZZZZZZZ";
	process (Clk)
	begin
		if Clk'event and Clk = '1' then
			if Instruction(13 downto 0) = "00000001100010" or
				to_integer(unsigned(File_Addr_r(7 downto 0))) = 129 then
				OPTION <= Res_Bus;
			end if;
			Old_B <= Port_B(7 downto 4);
			Old_INT <= INT;
		end if;
	end process;

	-- Interrupt Register
	Int_Trig <= (INTCON(0) and INTCON(3)) or
				(INTCON(1) and INTCON(4)) or
				(INTCON(2) and INTCON(5));
	Op_Bus <= INTCON when
			to_integer(unsigned(File_Addr_r(7 downto 0))) = 11 and
			File_Rd = '1' else "ZZZZZZZZ";
	process (Reset_s_n, Clk)
	begin
		if Reset_s_n = '0' then
			INTCON <= (others => '0');
		elsif Clk'event and Clk = '1' then
			if to_integer(unsigned(File_Addr_r(7 downto 0))) = 11 then
				INTCON <= Res_Bus;
			end if;
			if Int_Acc = '1' then
				INTCON(7) <= '0';
			end if;
			if Int_Ret = '1' then
				INTCON(7) <= '0';
			end if;
			if TOF = '1' then
				INTCON(2) <= '1';
			end if;
			if (OPTION(6) = '1' and INT = '1' and Old_INT = '0') or
				(OPTION(6) = '0' and INT = '0' and Old_INT = '1') then
				INTCON(1) <= '1';
			end if;
			if Old_B /= Port_B(7 downto 4) then
				INTCON(0) <= '1';
			end if;
		end if;
	end process;

	rom : ROM84 port map(
			Clk => Clk,
			A => ROM_Addr,
			D => ROM_Data);

	ppx : PPX16
		generic map(
			InstructionLength => InstructionLength,
			ROMAddressWidth => ROMAddressWidth,
			StackAddrWidth => StackAddrWidth,
			TopBoot => TopBoot)
		port map(
			Clk => Clk,
			Reset_n => Reset_s_n,
			ROM_Addr => ROM_Addr,
			ROM_Data => ROM_Data,
			Int_Trig => Int_Trig,
			GIE => INTCON(7),
			Int_Acc => Int_Acc,
			Int_Ret => Int_Ret,
			File_Addr => File_Addr,
			File_Addr_r => File_Addr_r,
			File_Rd => File_Rd,
			File_Wr => File_Wr,
			Instruction => Instruction,
			Op_Bus => Op_Bus,
			Res_Bus => Res_Bus);

	tmr0 : PPX_TMR
		port map(
			Clk => Clk,
			Reset_n => Reset_s_n,
			CKI => T0CKI,
			SE => OPTION(4),
			CS => OPTION(5),
			PS => OPTION(2 downto 0),
			PSA => OPTION(3),
			TMR_Sel => TMR_CS,
			Rd => File_Rd,
			Wr => File_Wr,
			Data_In => Res_Bus,
			Data_Out => Op_Bus,
			TOF => TOF);

	porta : PPX_Port
		port map(
			Clk => Clk,
			Reset_n => Reset_s_n,
			Port_CS => File_CS(5),
			Rd => File_Rd,
			Wr => File_Wr,
			Tris_Rd => Tris_A_Rd,
			Tris_Wr => Tris_A_Wr,
			Data_In => Res_Bus,
			Data_Out => Op_Bus,
			IOPort  => Port_A);

	portb : PPX_Port
		port map(
			Clk => Clk,
			Reset_n => Reset_s_n,
			Port_CS => File_CS(6),
			Rd => File_Rd,
			Wr => File_Wr,
			Tris_Rd => Tris_B_Rd,
			Tris_Wr => Tris_B_Wr,
			Data_In => Res_Bus,
			Data_Out => Op_Bus,
			IOPort  => Port_B);

end;
