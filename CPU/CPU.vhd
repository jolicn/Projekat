library ieee;
use ieee.std_logic_1164.all;

use work.Pack.all;

entity Pipeline is

	port
	(
		CLK, reset	: in  std_logic;

		DBUS	: inout std_logic_vector(31 downto 0);
		
		PCinit: std_logic_vector(31 downto 0);
		INIT: std_logic;
		
		wrEnable : in std_logic;

		ABUS	: out std_logic_vector(31 downto 0);
		
			
		wait_mem: out std_logic;
		mem_read: out std_logic
		
	);
end Pipeline;



architecture pipe of Pipeline is

	component Arbitrary
	
	port(
	
	IF_Instruction_Mem_req: in std_logic;
	
	EXE_Data_Mem_req: in std_logic;
	
	IF_Instruction_Mem_ack: out std_logic;
	
	EXE_Data_Mem_ack: out std_logic
	
	);
	
	end component;

	component IF_PHASE

		port
	(

		CLK, reset	: in  std_logic;

		mem_data	: inout std_logic_vector(31 downto 0);
		
		PCinit: std_logic_vector(31 downto 0);
		INIT: std_logic;
		
		PCout, PCnewout, IRout : out std_logic_vector(31 downto 0);

		mem_addr	: out std_logic_vector(31 downto 0); -- bez ; treba
		
		--TEMP
	
		wrEnable : in std_logic;

		brnc_cond_in : in std_logic;
		JMPaddr : in std_logic_vector(31 downto 0);
		BBL_flag_in : in std_logic;
		
		req: out std_logic;
		ack: in std_logic;
		
		wait_mem: out std_logic;
		mem_read: out std_logic

	);

	end component;

	component ID_PHASE

		port
		(
			clk, reset : in std_logic;
			IFID_PC, IFID_PCnew, IFID_IR : in std_logic_vector(31 downto 0);
			wr_addr_A	:	in std_logic_vector(3 downto 0);	
			wr_addr_B	:	in std_logic_vector(3 downto 0);
			
			data_A, data_B : in std_logic_vector(31 downto 0);
			swap : in std_logic;
			IRout : out std_logic_vector(31 downto 0);
			DP_I_flag_out, DP_R_flag_out, DP_flag_out : out std_logic;
			LS_flag_out, BBL_flag_out, S_flag_out : out std_logic;
			instruction_out : out instruction_type;
			data_Rnout, data_Rdout, data_Rmout, data_Rsout : out std_logic_vector(31 downto 0);
			PCout, PCnewOut : out std_logic_vector(31 downto 0)
		);

	end component;
	
	component EXE_PHASE
	
		port
		(
		clk, reset : in std_logic;
		instruction_in : in instruction_type;
		IRin : in std_logic_vector(31 downto 0);
		DP_I_flag_in, DP_R_flag_in, DP_flag_in : in std_logic;
		LS_flag_in, BBL_flag_in, S_flag_in : in std_logic;
		PC_in, PCnew_in : in std_logic_vector(31 downto 0);
		Rn_in, Rd_in, Rm_in, Rs_in : in std_logic_vector(31 downto 0);
		
		ALUout_out : out std_logic_vector(31 downto 0);
		swap : out std_logic;
		brnc_cond_out : out std_logic;
		JMPaddr : out std_logic_vector(31 downto 0);
		BBL_flag_out : out std_logic;
		IRout : out std_logic_vector(31 downto 0);
		LS_Rd_out, LS_addr_out : out std_logic_vector(31 downto 0);
		LS_l_out : out std_logic;
		PCnew_out : out std_logic_vector(31 downto 0);
		addrAtoBEXE, addrBtoAEXE : out std_logic_vector(3 downto 0);
		swapAtoB, swapBtoA : out std_logic_vector(31 downto 0)
		);
		
	end component;
	
	component MEM_PHASE
	
			port
			(
				clk, reset : in std_logic;
				IRin : in std_logic_vector(31 downto 0);
				ALUout_in : in std_logic_vector(31 downto 0);
				
				LS_addr, LS_data : in std_logic_vector(31 downto 0);
				LS_l : in std_logic;
				
				PCnew : in std_logic_vector(31 downto 0);
				
				IRout : out std_logic_vector(31 downto 0);
				ALUout_out : out std_logic_vector(31 downto 0);
				PCnew_out : out std_logic_vector(31 downto 0);
				
				Load_out : out std_logic_vector(31 downto 0)
					
			);
	
	end component;
	
	signal PCouttmp, PCnewouttmp, IRouttmp : std_logic_vector(31 downto 0);
	signal Rn, Rd, Rm, Rs : std_logic_vector(31 downto 0);
	signal DP_I_flag, DP_R_flag, DP_flag : std_logic;
	signal LS_flag, BBL_flag, S_flag : std_logic;
	signal instr : instruction_type;
	signal PCoutIDEXE, PCnewoutIDEXE, IRoutIDEXE : std_logic_vector(31 downto 0);

	signal brnc_cond_outEXE : std_logic;

	signal LS_Rd_out, LS_addr_out : std_logic_vector(31 downto 0);
	signal LS_l_out : std_logic;
	signal PCnew_out : std_logic_vector(31 downto 0);
	signal swapAtoB, swapBtoA : std_logic_vector(31 downto 0);
	signal req_if,ack_if : std_logic;
	signal req_exe,ack_exe : std_logic;
	signal brnc_condEXE_IF :  std_logic;
	signal JMPaddrEXE_IF :  std_logic_vector(31 downto 0);
	signal BBL_flagEXE_IF :  std_logic;
	signal swapAtoB_signal, swapBtoA_signal : std_logic_vector(31 downto 0);
	signal addrAtoB_sig, addrBtoA_sig : std_logic_vector(3 downto 0);
	signal swapEXE_ID : std_logic;
	signal IRoutEXE_MEM : std_logic_vector(31 downto 0);
	signal ALUout_EXE_MEM : std_logic_vector(31 downto 0);
				
	signal LS_addr_EXE_MEM, LS_data_EXE_MEM : std_logic_vector(31 downto 0);
	signal LS_l_EXE_MEM : std_logic;
				
	signal PCnew_EXE_MEM : std_logic_vector(31 downto 0);
	
	signal IRout_MEM : std_logic_vector(31 downto 0);
	signal ALUout_out_MEM : std_logic_vector(31 downto 0);
	signal PCnew_out_MEM : std_logic_vector(31 downto 0);
				
	signal Load_out_MEM : std_logic_vector(31 downto 0);
	
begin

	IF_faza : IF_PHASE port map (
		CLK => CLK, reset => reset,
			
		PCinit => PCinit,
		INIT => INIT,
			
		PCout => PCouttmp, PCnewout => PCnewouttmp, IRout => IRouttmp,
		
		brnc_cond_in => brnc_condEXE_IF,
		JMPaddr => JMPaddrEXE_IF,
		BBL_flag_in => BBL_flagEXE_IF,
	
		mem_addr => ABUS, -- bez ; treba
		mem_data	=> DBUS,
		req => req_if,
		ack => ack_if,
		wait_mem => wait_mem,
		mem_read => mem_read,
			--TEMP
		
		wrEnable => wrEnable
	);
	
	ID_faza : ID_PHASE port map (
		
		clk => clk, reset => reset,
		wr_addr_A => addrBtoA_sig,
		wr_addr_B => addrAtoB_sig,
		
		data_A => swapBtoA_signal, data_B => swapAtoB_signal,
		swap => swapEXE_ID,
		IFID_PC => PCouttmp, IFID_PCnew => PCnewouttmp, IFID_IR => IRouttmp,
			
		DP_I_flag_out => DP_I_flag, DP_R_flag_out => DP_R_flag, DP_flag_out => DP_flag,
		LS_flag_out => LS_flag, BBL_flag_out => BBL_flag, S_flag_out => S_flag,
		IRout => IRoutIDEXE,
		instruction_out => instr,
		data_Rnout => Rn, data_Rdout => Rd, data_Rmout => Rm, data_Rsout => Rs,
		PCout => PCoutIDEXE, PCnewOut => PCnewoutIDEXE
	);
	
	EXE_faza : EXE_PHASE port map (
		clk => clk, reset => reset,
		instruction_in => instr,
		IRin => IRoutIDEXE,
		DP_I_flag_in => DP_I_flag, DP_R_flag_in => DP_R_flag, DP_flag_in => DP_flag,
		LS_flag_in => LS_flag, BBL_flag_in => BBL_flag, S_flag_in => S_flag,
		PC_in => PCoutIDEXE, PCnew_in => PCnewoutIDEXE,
		Rn_in => Rn, Rd_in => Rd, Rm_in => Rm, Rs_in => Rs,
		
		ALUout_out => ALUout_EXE_MEM,
		swap => swapEXE_ID,
		brnc_cond_out => brnc_condEXE_IF,
		JMPaddr => JMPaddrEXE_IF,
		BBL_flag_out => BBL_flagEXE_IF,
		IRout => IRoutEXE_MEM,
		LS_Rd_out => LS_data_EXE_MEM, LS_addr_out => LS_addr_EXE_MEM,
		LS_l_out => LS_l_EXE_MEM,
		PCnew_out => PCnew_EXE_MEM,
		addrAtoBEXE => addrAtoB_sig, addrBtoAEXE => addrBtoA_sig,
		swapAtoB => swapAtoB_signal, swapBtoA => swapBtoA_signal
	);
	
	MEM_faza : MEM_PHASE port map (
	
		clk => clk, reset => reset,
		IRin => IRoutEXE_MEM,
		ALUout_in => ALUout_EXE_MEM,
				
		LS_addr => LS_addr_EXE_MEM, LS_data => LS_data_EXE_MEM,
		LS_l => LS_l_EXE_MEM,
				
		PCnew => PCnew_EXE_MEM,
		
				--------------------
				
		IRout => IRout_MEM,
		ALUout_out => ALUout_out_MEM,
		PCnew_out => PCnew_out_MEM,
				
		Load_out => Load_out_MEM

	);
	
	
	Arbitrator : Arbitrary port map (
	
		IF_Instruction_Mem_req => req_if,
		
		EXE_Data_Mem_ack => ack_exe,
	
		IF_Instruction_Mem_ack => ack_if,
		
		EXE_Data_Mem_req => req_exe
	
		
	
	);


end pipe;
