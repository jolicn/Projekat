library ieee;
use ieee.std_logic_1164.all;

entity IF_PHASE is

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
end entity;

architecture structural of IF_PHASE is

		component IF_PC

			port
			(
				CLK, reset	: in  std_logic;
					
				brnc_cond_in : in std_logic;
				JMPaddr : in std_logic_vector(31 downto 0);
				BBL_flag_in : in std_logic;
				
				PCinit	: in  std_logic_vector(31 downto 0);
				INIT : in std_logic;

				PC	: out std_logic_vector(31 downto 0);
				PCnew: out std_logic_vector(31 downto 0)
			);

		end component;
		
		component instruction_cache_mem
		
			port
			(
				address: in std_logic_vector(31 downto 0); -- adresa u kesu
				data_out: out std_logic_vector(31 downto 0); --procitani podatak
		
				reset	: in  std_logic; 
				clk	: in  std_logic;

				-- indicates that cache is working with MEM
				-- generates STALL signal
				wait_mem: out std_logic;
	
				-- ABUS and DBUS
				mem_addr: out std_logic_vector(31 downto 0);
				mem_data: inout std_logic_vector(31 downto 0);
	
				-- memory read signal
				mem_read: out std_logic;
	
				-- request and acknowledgement for MEM access
				req: out std_logic;
				ack: in std_logic
				);
		end component;
		
		component IF_ID
		
			port
			(
				clk, reset	: in  std_logic;
				PCin, PCnewIn	: in  std_logic_vector(31 downto 0);
				IRin	: in std_logic_vector(31 downto 0);

				PCout, PCnewOut	: out std_logic_vector(31 downto 0);
				IRout	: out std_logic_vector(31 downto 0)
			);
		end component;
	
	signal PCtmp: std_logic_vector(31 downto 0);
	signal PCnewtmp: std_logic_vector(31 downto 0);
	signal data_outIF : std_logic_vector(31 downto 0);	-- ovo ide u vazduh
	

begin

		IF_PC_reg : IF_PC port map (
			CLK => CLK, reset => reset, PCinit => PCinit, INIT => INIT,			
			JMPaddr => JMPaddr,
			brnc_cond_in => brnc_cond_in,
			bbl_flag_in => bbl_flag_in,
			PC => PCtmp, PCnew => PCnewtmp
		);
		
		instruction_mem : instruction_cache_mem port map (
			clk => CLK, reset => RESET, ack => ack, req => req, address => PCtmp, mem_addr=>mem_addr, mem_data => mem_data,
			data_out => data_outIF, wait_mem => wait_mem , mem_read => mem_read
		);
		
		IF_ID_stanglica : IF_ID port map (
			clk => CLK, reset => reset, PCin => PCtmp, PCnewIn => PCnewtmp,
			IRin => data_outIF, PCout => PCout, PCnewOut => PCnewout, IRout => IRout
		);
end architecture;
