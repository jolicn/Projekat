library ieee;
use ieee.std_logic_1164.all;

entity LoadStore is

	port
	(
		Rn, Rd : in std_logic_vector(31 downto 0);
		l : in std_logic;
		mem_address : out std_logic_vector(31 downto 0);
		Rd_out : out std_logic_vector(31 downto 0)
	);
end LoadStore;

architecture behavioral of LoadStore is

begin

	mem_address <= Rn;
	Rd_out <= Rd;
	
end behavioral;

