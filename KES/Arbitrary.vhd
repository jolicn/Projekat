library ieee;
use ieee.std_logic_1164.all;

entity Arbitrary is
port(
	IF_Instruction_Mem_req: in std_logic;
	EXE_Data_Mem_req: in std_logic;
	IF_Instruction_Mem_ack: out std_logic;
	EXE_Data_Mem_ack: out std_logic
);
end entity;

architecture behavior of Arbitrary is
begin
	process(IF_Instruction_Mem_req, EXE_Data_Mem_req) is
	begin
		if(EXE_Data_Mem_req='1') then
			EXE_Data_Mem_ack<='1';
			IF_Instruction_Mem_ack<='0';
		elsif(IF_Instruction_Mem_req='1') then
			EXE_Data_Mem_ack<='0';
			IF_Instruction_Mem_ack<='1';
		else
			EXE_Data_Mem_ack<='0';
			IF_Instruction_Mem_ack<='0';
		end if;
	end process;
end architecture behavior;