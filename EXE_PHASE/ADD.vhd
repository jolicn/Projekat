library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;

entity ADD is

	port
	(
		a, b : in std_logic_vector(31 downto 0);
		cin : in std_logic;
		
		sum : out std_logic_vector(31 downto 0);
		cout : out std_logic;
		vout : out std_logic
		
	);
end ADD;


architecture behavioral of ADD is

constant i_cin_ext : std_logic_vector(32 downto 1) := (others => '0');
signal i_sum : std_logic_vector(32 downto 0);

begin

	
	process(a, b, cin) is
		
	begin
		i_sum <= ('0'& a) + ('0'& b) + (i_cin_ext & cin);
		sum <= i_sum(31 downto 0);
		cout <= i_sum(31);
		vout <= (a(31) and b(31) and (not i_sum(31))) or
					(not a(31) and not b(31) and i_sum(31));
		
		
	end process;



end behavioral;

