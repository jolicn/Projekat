library ieee;
use ieee.std_logic_1164.all;

use work.Pack.all;

entity ID_EXE is

	port
	(
		clk, reset : in std_logic;
		
		instruction_in : in instruction_type;
		instruction_out : out instruction_type;
		
		IRin : in std_logic_vector(31 downto 0);
		IRout : out std_logic_vector(31 downto 0);
		
		DP_I_flag_in, DP_R_flag_in, DP_flag_in : in std_logic;
		LS_flag_in, BBL_flag_in, S_flag_in : in std_logic;
		
		DP_I_flag_out, DP_R_flag_out, DP_flag_out : out std_logic;
		LS_flag_out, BBL_flag_out, S_flag_out : out std_logic;
		
		PC_in, PCnew_in : in std_logic_vector(31 downto 0);
		PC_out, PCnew_out : out std_logic_vector(31 downto 0);
		
		Rn_in, Rd_in, Rm_in, Rs_in : in std_logic_vector(31 downto 0);
		Rn_out, Rd_out, Rm_out, Rs_out : out std_logic_vector(31 downto 0)
		
	);
end ID_EXE;


architecture behavioral of ID_EXE is


begin
	process (clk, reset) is
	
	begin
		if (reset = '1') then
			PC_out <= x"00000000";
			PCnew_out <= x"00000000";
			Rn_out <= x"00000000";
			Rd_out <= x"00000000";
			Rm_out <= x"00000000";
			Rs_out <= x"00000000";
			IRout <= x"00000000";
			DP_I_flag_out <= '0';
			DP_R_flag_out <= '0';
			DP_flag_out <= '0';
			LS_flag_out <= '0';
			BBL_flag_out <= '0';
			S_flag_out <= '0';
		elsif (rising_edge(clk)) then
			PC_out <= PC_in;
			PCnew_out <= PCnew_in;
			Rn_out <= Rn_in;
			Rd_out <= Rd_in;
			Rm_out <= Rm_in;
			Rs_out <= Rs_in;
			instruction_out <= instruction_in;
			IRout <= IRin;
			DP_I_flag_out <= DP_I_flag_in;
			DP_R_flag_out <= DP_R_flag_in;
			DP_flag_out <= DP_flag_in;
			LS_flag_out <= LS_flag_in;
			BBL_flag_out <= BBL_flag_in;
			S_flag_out <= S_flag_in;
		end if;
	end process;
	
	


end behavioral;
