library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ImmedOp is

	port
	(
		IRin : in std_logic_vector(31 downto 0);
		s : in std_logic;
		Imm : out std_logic_vector(31 downto 0)
	);
	
end ImmedOp;


architecture arch of ImmedOp is


begin

		Imm <= 	std_logic_vector(resize(signed(IRin(15 downto 0)), 32)) when s = '0' else
					std_logic_vector(resize(unsigned(IRin(15 downto 0)), 32)) when s = '1';

end arch;
