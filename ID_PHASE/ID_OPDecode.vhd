library ieee;
use ieee.std_logic_1164.all;

use work.Pack.all;

entity ID_OPDecode is
	port
	(
		IR : in std_logic_vector(31 downto 0);
		
		DP_I_flag, DP_R_flag, DP_flag : out std_logic;
		LS_flag, BBL_flag, S_flag : out std_logic;
		instruction : out instruction_type
		
	);
end ID_OPDecode;


architecture behavioral of ID_OPDecode is



begin

	process(IR) is 

	begin 
		
		DP_I_flag <= '0';
		DP_R_flag <= '0';
		DP_flag <= '0';
		LS_flag <= '0';
		BBL_flag <= '0';
		S_flag <= '0';
		
		if (IR(31 downto 30) = "00") then								-- DP_R ili DP_I
			
			if (IR(29) = '0') then												-- DP_R
				instruction <= DP_R;
				DP_R_flag <= '1';
				
			elsif (IR(29) = '1') then
				instruction <= DP_I;												-- DP_I
				DP_I_flag <= '1';
				
			end if;
			
			DP_flag <= '1';
			
		elsif (IR(31 downto 29) = "010") then							-- LS
			instruction <= LS;
			LS_flag <= '1';
			
		elsif (IR(31 downto 29) = "100") then							-- BBL
			instruction <= BBL;	
			BBL_flag <= '1';
		
		elsif (IR(31 downto 29) = "101") then							-- S
			instruction <= S;	
			S_flag <= '1';
		
		end if;
	
	
	
	end process; 


end behavioral;