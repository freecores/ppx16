--
-- PIC16xx compatible microcontroller core
--
-- Version : 0146
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
--	Registers implemented in this entity are INDF, PCL, STATUS, FSR, (PCLATH)
--	other registers must be implemented externally including GPR
--
-- File history :
--

library IEEE;
use IEEE.std_logic_1164.all;

entity PPX_Port is
	port(
		Clk			: in std_logic;
		Reset_n		: in std_logic;
		Port_CS		: in std_logic;
		Rd			: in std_logic;
		Wr			: in std_logic;
		Tris_Rd		: in std_logic;
		Tris_Wr		: in std_logic;
		Data_In		: in std_logic_vector(7 downto 0);
		Data_Out	: out std_logic_vector(7 downto 0);
		IOPort		: inout std_logic_vector(7 downto 0)
	);
end PPX_Port;

architecture rtl of PPX_Port is

	signal Tris			: std_logic_vector(7 downto 0);
	signal Port_Output	: std_logic_vector(7 downto 0);
	signal Port_Input	: std_logic_vector(7 downto 0);

begin

	IOPort(0) <= Port_Output(0) when Tris(0) = '0' else 'Z';
	IOPort(1) <= Port_Output(1) when Tris(1) = '0' else 'Z';
	IOPort(2) <= Port_Output(2) when Tris(2) = '0' else 'Z';
	IOPort(3) <= Port_Output(3) when Tris(3) = '0' else 'Z';
	IOPort(4) <= Port_Output(4) when Tris(4) = '0' else 'Z';
	IOPort(5) <= Port_Output(5) when Tris(5) = '0' else 'Z';
	IOPort(6) <= Port_Output(6) when Tris(6) = '0' else 'Z';
	IOPort(7) <= Port_Output(7) when Tris(7) = '0' else 'Z';

	Data_Out <= Port_Input when Port_CS = '1' and Rd = '1' else "ZZZZZZZZ";

	process (Clk)
	begin
		if Clk'event and Clk = '1' then
			Port_Input <= IOPort;	-- Synchronise input
			if Port_CS = '1' and Wr = '1' then
				Port_Output <= Data_In;
				Port_Input <= Data_In;
			end if;
		end if;
	end process;

	Data_Out <= Tris when Tris_Rd = '1' else "ZZZZZZZZ";

	process (Reset_n, Clk)
	begin
		if Reset_n = '0' then
			Tris <= "11111111";
		elsif Clk'event and Clk = '1' then
			if Tris_Wr = '1' then
				Tris <= Data_In;
			end if;
		end if;
	end process;

end;
