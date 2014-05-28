library ieee;
use ieee.std_logic_1164.all;

entity regmem is

	port
	(
		clk : in std_logic;
		data_in : in std_logic_vector(31 downto 0);
		data_out : out std_logic_vector(31 downto 0);
		wr,rd: in std_logic;
		cs:in std_logic;
	   address: in std_logic_vector (12 downto 0);
		wrout,rdout: out std_logic;
		csout:out std_logic;
		addressout: out std_logic_vector (12 downto 0)
	);
end regmem;



architecture arch of regmem is


begin

	process(clk) is
	begin
			if (rising_edge(clk)) then
				data_out <= data_in;
				wrout<=wr;
				rdout<=rd;
				csout<=cs;
				addressout<= address;
			end if;
	end process;


end arch;