library ieee;
use ieee.std_logic_1164.all;

use work.Pack.all;

entity ID_PHASE is
	port
	(
		clk, reset : in std_logic;
		IFID_PC, IFID_PCnew, IFID_IR : in std_logic_vector(31 downto 0);
		
		wr_addr_A	:	in std_logic_vector(3 downto 0);	
		wr_addr_B	:	in std_logic_vector(3 downto 0);
		
		data_A, data_B : in std_logic_vector(31 downto 0);
		swap : in std_logic;
		
		DP_I_flag_out, DP_R_flag_out, DP_flag_out : out std_logic;
		LS_flag_out, BBL_flag_out, S_flag_out : out std_logic;
		IRout : out std_logic_vector(31 downto 0);
		instruction_out : out instruction_type;
		data_Rnout, data_Rdout, data_Rmout, data_Rsout : out std_logic_vector(31 downto 0);
		PCout, PCnewOut : out std_logic_vector(31 downto 0)
		
	);
end ID_PHASE;



architecture structural of ID_PHASE is
	

	component Reg_file

			port
			(
			clk, reset : in std_logic;
			
			wr_addr_A	:	in std_logic_vector(3 downto 0);
			wr_addr_B	:	in std_logic_vector(3 downto 0);
			
			data_A, data_B : in std_logic_vector(31 downto 0);
			swap : in std_logic;
			
			instr_type : in instruction_type;
			
			IFID_PC, IFID_IR : in std_logic_vector(31 downto 0);
			
			data_Rn	: out std_logic_vector(31 downto 0);
			data_Rd	: out std_logic_vector(31 downto 0);
			data_Rm	: out std_logic_vector(31 downto 0);
			data_Rs	: out std_logic_vector(31 downto 0)
		);

	end component;


	component ID_OPDecode

		port
		(
			IR : in std_logic_vector(31 downto 0);
			
			DP_I_flag, DP_R_flag, DP_flag : out std_logic;
			LS_flag, BBL_flag, S_flag : out std_logic;
			instruction : out instruction_type
		);

	end component;
	
	component ID_EXE
		
		port 
		(
			clk, reset	: in  std_logic;
			
			instruction_in : in instruction_type;
			instruction_out : out instruction_type;
			
			IRin : in std_logic_vector(31 downto 0);
			IRout : out std_logic_vector(31 downto 0);
			
			DP_I_flag_in, DP_R_flag_in, DP_flag_in : in std_logic;
			LS_flag_in, BBL_flag_in, S_flag_in : in std_logic;
			
			DP_I_flag_out, DP_R_flag_out, DP_flag_out : out std_logic;
			LS_flag_out, BBL_flag_out, S_flag_out : out std_logic;
			
			PC_in, PCnew_in	: in  std_logic_vector(31 downto 0);
			
			PC_out, PCnew_out	: out std_logic_vector(31 downto 0);
			
			Rn_in, Rd_in, Rm_in, Rs_in : in std_logic_vector(31 downto 0);
			Rn_out, Rd_out, Rm_out, Rs_out : out std_logic_vector(31 downto 0)

		);
	end component;
	
	signal instr_type : instruction_type;
	signal Rn, Rd, Rm, Rs : std_logic_vector(31 downto 0);
	signal instr : instruction_type;
	signal DP_I_flag, DP_R_flag, DP_flag : std_logic;
	signal LS_flag, BBL_flag, S_flag : std_logic;
	

begin

	RegFile : Reg_file port map (
		clk => clk, reset => reset, 
		wr_addr_A => wr_addr_A,
		wr_addr_B => wr_addr_B,
			
		data_A => data_A, data_B => data_B,
		swap => swap,
		
		instr_type => instr_type,
		IFID_PC => IFID_PC, IFID_IR => IFID_IR,	
		data_Rn	=> Rn,
		data_Rd	=> Rd,
		data_Rm	=> Rm,
		data_Rs	=> Rs
	);
	
	OPDecode : ID_OPDecode port map (
		IR => IFID_IR, 
		DP_I_flag => DP_I_flag, DP_R_flag => DP_R_flag, DP_flag => DP_flag,
		LS_flag => LS_flag, BBL_flag => BBL_flag, S_flag => S_flag,
		instruction => instr_type
	);
	
	ID_EXE_stanglica : ID_EXE port map (
		clk => clk, reset => reset,
		instruction_in => instr_type, instruction_out => instruction_out,
		
			IRin => IFID_IR,
			IRout => IRout,
			
			DP_I_flag_in => DP_I_flag, DP_R_flag_in => DP_R_flag, DP_flag_in => DP_flag,
			LS_flag_in => LS_flag, BBL_flag_in => BBL_flag, S_flag_in => S_flag,
			
			DP_I_flag_out => DP_I_flag_out, DP_R_flag_out => DP_R_flag_out, DP_flag_out => DP_flag_out,
			LS_flag_out => LS_flag_out, BBL_flag_out => BBL_flag_out, S_flag_out => S_flag_out,
		
		PC_in => IFID_PC, PCnew_in => IFID_PCnew,

		PC_out => PCout, PCnew_out => PCnewOut,
		
		Rn_in => Rn, Rd_in => Rd, Rm_in => Rm, Rs_in => Rs,
		Rn_out => data_Rnout, Rd_out => data_Rdout, Rm_out => data_Rmout, Rs_out => data_Rsout

	);

end structural;
