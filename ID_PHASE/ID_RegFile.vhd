library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.Pack.all;

entity Reg_file is
	port
	(
		clk, reset : in std_logic;
		
		wr_addr_A	:	in std_logic_vector(3 downto 0);
		wr_addr_B	:	in std_logic_vector(3 downto 0);
		
		data_A, data_B : in std_logic_vector(31 downto 0);
		swap : in std_logic;
		
		instr_type : in instruction_type;
		
		IFID_PC, IFID_IR : in std_logic_vector(31 downto 0);
		
		data_Rn	: out std_logic_vector(31 downto 0);
		data_Rd	: out std_logic_vector(31 downto 0);
		data_Rm	: out std_logic_vector(31 downto 0);
		data_Rs	: out std_logic_vector(31 downto 0)
	
	);
end Reg_file;


architecture behavioral of Reg_file is

	
	type reg_type is array (15 downto 0) of std_logic_vector(31 downto 0);
	signal registers : reg_type;
	
		
	signal rd_addr_Rn	: std_logic_vector(3 downto 0);
	signal rd_addr_Rd	: std_logic_vector(3 downto 0);
	signal rd_addr_Rm	: std_logic_vector(3 downto 0);
	signal rd_addr_Rs	: std_logic_vector(3 downto 0);


begin
	
	
	process (reset, clk, instr_type, IFID_PC, IFID_IR) is
	
	begin
		
		if(reset = '1') then
			for i in 0 to 15 loop
				registers(i) <= x"00000000";
			end loop;
		elsif(rising_edge(clk)) then
			if (swap = '1') then
				registers(to_integer(unsigned(wr_addr_A))) <= data_A;
				registers(to_integer(unsigned(wr_addr_B))) <= data_B;
			end if;
		end if;
		
		data_Rn	<= (others => '0');
		data_Rd	<= (others => '0');
		data_Rm	<= (others => '0');
		data_Rs	<= (others => '0');
		
		if (instr_type = DP_R) then
			
			rd_addr_Rn <= IFID_IR(24 downto 21);
			rd_addr_Rd <= IFID_IR(20 downto 17);
			rd_addr_Rm <= IFID_IR(15 downto 12);
			rd_addr_Rs <= IFID_IR(11 downto 8);
			
		elsif (instr_type = DP_I) then
		
			rd_addr_Rn <= IFID_IR(24 downto 21);
			rd_addr_Rd <= IFID_IR(20 downto 17);
			rd_addr_Rm <= "0000";
			rd_addr_Rs <= "0000";
			
		elsif (instr_type = LS) then
	
			rd_addr_Rn <= IFID_IR(24 downto 21);
			rd_addr_Rd <= IFID_IR(20 downto 17);
			rd_addr_Rm <= "0000";
			rd_addr_Rs <= "0000";
		
		elsif (instr_type = BBL or instr_type = S) then
		
			rd_addr_Rn <= "0000";
			rd_addr_Rd <= "0000";
			rd_addr_Rm <= "0000";
			rd_addr_Rs <= "0000";
			
		end if;
		
		data_Rn <= registers(to_integer(unsigned(rd_addr_Rn)));
		data_Rd <= registers(to_integer(unsigned(rd_addr_Rd)));
		data_Rm <= registers(to_integer(unsigned(rd_addr_Rm)));
		data_Rs <= registers(to_integer(unsigned(rd_addr_Rs)));
		
	end process;


end behavioral;
