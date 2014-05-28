library ieee;
use ieee.std_logic_1164.all;
use IEEE.STD_LOGIC_SIGNED.all;

entity CSR is

	port
	(
		CLK, reset	: in  std_logic;
		
		N, Z, V, C : in std_logic;


		output : out std_logic_vector(31 downto 0)
	);
end entity;

architecture Behavioral of CSR is

	signal CSRnext, CSRreg : std_logic_vector(31 downto 0);
	signal input : std_logic_vector(31 downto 0);

begin

	process (CLK, reset, N, Z, C, V)
	
		begin
			input <= (others => '0');
			input(31) <= N;
			input(30) <= Z;
			input(29) <= C;
			input(28) <= V;
			CSRnext <= input;
			
			if (reset = '1') then
				CSRreg <= x"00000000";
			elsif (CLK'event and CLK = '1') then
				CSRreg <= CSRnext;
			end if;
			
			output <= CSRreg;
		end process;

end architecture;


