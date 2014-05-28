library ieee;
use ieee.std_logic_1164.all;

entity IF_ID is
	port
	(
		clk, reset	: in  std_logic;
		PCin, PCnewIn	: in  std_logic_vector(31 downto 0);
		IRin	: in std_logic_vector(31 downto 0);

		PCout, PCnewOut	: out std_logic_vector(31 downto 0);
		IRout	: out std_logic_vector(31 downto 0)

	);
end IF_ID;


architecture behavioral of IF_ID is


begin

	process(reset, clk) is 

	begin 
		if(reset = '1') then
			PCout <= x"00000000";
			PCnewOut <= x"00000000";
			IRout <= x"00000000";
		elsif(rising_edge(clk)) then
			PCout <= PCin;
			PCnewOut <= PCnewIn;
			IRout <= IRin;
		end if;
	end process; 

end behavioral;
 
