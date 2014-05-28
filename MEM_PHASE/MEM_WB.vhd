library ieee;
use ieee.std_logic_1164.all;
----
----
entity MEM_WB is

	port
	(	
		clk, reset : in std_logic;
		IRin : in std_logic_vector(31 downto 0);
		ALUout_in : in std_logic_vector(31 downto 0);
		
		Load_out_in : in std_logic_vector(31 downto 0);
		
		PCnew : in std_logic_vector(31 downto 0);
		
		--------------------
		
		IRout : out std_logic_vector(31 downto 0);
		ALUout_out : out std_logic_vector(31 downto 0);
		PCnew_out : out std_logic_vector(31 downto 0);
		
		Load_out : out std_logic_vector(31 downto 0)
		
	);
end MEM_WB;

architecture behavioral of MEM_WB is

begin

	process (clk, reset) is
	
	begin
		if (reset = '1') then
			IRout  <= x"00000000";
			ALUout_out  <= x"00000000";
			PCnew_out  <= x"00000000";
			
			Load_out  <= x"00000000";
		elsif (rising_edge(clk)) then
			IRout  <= IRin;
			ALUout_out  <= ALUout_in;
			PCnew_out  <= PCnew;
			
			Load_out  <= Load_out_in;
		
		
		end if;
		
	end process;

end behavioral;
