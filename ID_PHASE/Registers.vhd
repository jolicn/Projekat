library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity Regs is

	port
	(
		clk, reset : in std_logic;
		wr_addr_A	:	in std_logic_vector(3 downto 0);
		wr_addr_B	:	in std_logic_vector(3 downto 0);
		
		rd_addr_Rn	:	in std_logic_vector(3 downto 0);
		rd_addr_Rd	:	in std_logic_vector(3 downto 0);
		rd_addr_Rm	:	in std_logic_vector(3 downto 0);
		rd_addr_Rs	:	in std_logic_vector(3 downto 0);
		
		data_in 	:	in	std_logic_vector(31 downto 0);

--		wre : in std_logic;

		-- Output ports
		data_Rn	: out std_logic_vector(31 downto 0);
		data_Rd	: out std_logic_vector(31 downto 0);
		data_Rm	: out std_logic_vector(31 downto 0);
		data_Rs	: out std_logic_vector(31 downto 0)

	);
end Regs;


architecture behavior of Regs is

	type reg_type is array (15 downto 0) of std_logic_vector(31 downto 0);
	signal registers : reg_type;
begin

	process(reset, clk) is 
		-- Declaration(s) 
	begin 
		if(reset = '1') then
			for i in 0 to 15 loop
				registers(i) <= x"00000000";
			end loop;
		elsif(rising_edge(clk)) then
--			if (wre = '1') then
				registers(to_integer(unsigned(addr_w))) <= data_in;
--			end if;
		end if;
	end process; 

	data_Rn <= registers(to_integer(unsigned(rd_addr_Rn)));
	data_Rd <= registers(to_integer(unsigned(rd_addr_Rd)));
	data_Rm <= registers(to_integer(unsigned(rd_addr_Rm)));
	data_Rs <= registers(to_integer(unsigned(rd_addr_Rs)));
end behavior;
