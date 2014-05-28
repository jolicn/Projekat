library ieee;
use ieee.std_logic_1164.all;

entity EXE_MEM is
	port
	(
		clk, reset : in std_logic;
		IRin : in std_logic_vector;
		ALUout, BBLaddr : in std_logic_vector(31 downto 0);
		brnc_cond : in std_logic;
		BBL_flag : in std_logic;
		LS_Rd, LS_addr : in std_logic_vector(31 downto 0);
		LS_l : in std_logic;
		PCnew : in std_logic_vector(31 downto 0);
		dest_PC_in : in std_logic;
		swapAtoB_in, swapBtoA_in : in std_logic_vector(31 downto 0);
		addrAtoB_in, addrBtoA_in : in std_logic_vector(3 downto 0);
		swap_in : in std_logic;
		
		swap_out : out std_logic;
		swapAtoB_out, swapBtoA_out : out std_logic_vector(31 downto 0);
		addrAtoB_out, addrBtoA_out : out std_logic_vector(3 downto 0);
		dest_PC_out : out std_logic;
		ALUout_out : out std_logic_vector(31 downto 0);
		JMPaddr : out std_logic_vector(31 downto 0);
		brnc_cond_out : out std_logic;
		BBL_flag_out : out std_logic;
		IRout : out std_logic_vector(31 downto 0);
		LS_Rd_out, LS_addr_out : out std_logic_vector(31 downto 0);
		LS_l_out : out std_logic;
		PCnew_out : out std_logic_vector(31 downto 0)
		
	);
end EXE_MEM;


architecture behavioral of EXE_MEM is

begin
	process (clk, reset) is
	
	begin
	
		if (reset = '1') then
--			ALUout_out <= x"00000000"; 
--			BBLaddr_out <= x"00000000";
			brnc_cond_out <= '0';
			BBL_flag_out <= '0';
			IRout <= x"00000000";
			LS_Rd_out <= x"00000000";
			LS_addr_out <= x"00000000";
			LS_l_out <= '0';
			PCnew_out <= x"00000000";
			dest_PC_out <= '0';
			swapAtoB_out <= x"00000000";
			swapBtoA_out <= x"00000000";
			addrAtoB_out <= x"0";
			addrBtoA_out <= x"0";
			swap_out <= '0';
		elsif (rising_edge(clk)) then
--			ALUout_out <= ALUout; 
--			BBLaddr_out <= BBLaddr;
			brnc_cond_out <= brnc_cond;
			BBL_flag_out <= BBL_flag or dest_PC_in;
			IRout <= IRin;
			LS_Rd_out <= LS_Rd;
			LS_addr_out <= LS_addr;
			LS_l_out <= LS_l;
			PCnew_out <= PCnew;
			dest_PC_out <= dest_PC_in;
			swapAtoB_out <= swapAtoB_in;
			swapBtoA_out <= swapBtoA_in;
			addrAtoB_out <= addrAtoB_in;
			addrBtoA_out <= addrBtoA_in;
			swap_out <= swap_in;
		end if;
		

	
	end process;
	
			JMPaddr <= ALUout when dest_PC_in = '1' else
						BBLaddr;

end behavioral;
