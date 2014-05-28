library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;

entity instruction_cache_mem is
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
end instruction_cache_mem;



architecture behavior of instruction_cache_mem is


type data_type is array (2**6-1 downto 0) of std_logic_vector(31 downto 0);
type tag_type is array (2**4-1 downto 0) of std_logic_vector(25 downto 0);

signal valid: std_logic_vector(15 downto 0);


signal data: data_type;
signal tag: tag_type;
	
	
type state_type is (IDLE, READ_MEM, READ_DATA_FROM_BUS, WAITING);
signal current_s, next_s : state_type;


--broj reci
signal num_of_words: std_logic_vector(1 downto 0);

--broji koliko memorija treba da se ceka
signal mem_wait: std_logic_vector(3 downto 0);

begin

process(reset, clk) is

variable addr: std_logic_vector(5 downto 0);
begin
			if (reset = '1') then 
			
				current_s <= IDLE;

				--inicijalizacija valid bita
				for i in 0 to 15 loop
				
					valid(i) <= '0';
				
				end loop;
			
			elsif (rising_edge(clk)) then 
				
				if(current_s = IDLE) then
					
					if(next_s = READ_MEM) then
					
						num_of_words <= B"01";
						
						else 
						
						num_of_words <= B"00";
						
					end if;
					
				elsif (current_s= READ_MEM) then
						
						if(next_s = READ_MEM) then
					
							num_of_words<= num_of_words+'1';
						
						else
						
							num_of_words <= B"00";
						
							mem_wait <= X"D";
						
						end if;
					
				elsif (current_s = WAITING) then
					
						if(next_s = WAITING) then
							
							mem_wait <= mem_wait - '1';
						
							num_of_words <= B"00";
							
						else
						
							mem_wait <= X"0";
							
							num_of_words <= B"00";
							
						end if;
					
						else 
						if (next_s = READ_DATA_FROM_BUS) then
						
							addr := address(5 downto 2) & num_of_words(1 downto 0);
							data(to_integer(unsigned(addr))) <= mem_data;
					
							num_of_words <= num_of_words + '1';
							
							else
							
							addr := address(5 downto 2) & num_of_words(1 downto 0);
							data(to_integer(unsigned(addr))) <= mem_data;
							
							tag(to_integer(unsigned(address(5 downto 2)))) <= address(31 downto 6);
							valid(to_integer(unsigned(address(5 downto 2)))) <= '1';
							
							end if;
							
						end if;
						
						current_s <= next_s;
			
						
		end if;				
						

	
end process;

process(current_s, tag, valid, data, address, num_of_words, mem_wait,ack, reset ) is

variable addr: std_logic_vector (5 downto 0);

begin
		
		next_s <= IDLE;
		
		--za komunikaciju sa memorijom
		wait_mem <= '0';
		mem_addr <= "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ";
		mem_read <= 'Z';
		
		req <= '0';
		
		data_out <= "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ";
		
		case current_s is
		-- u ovom stanju se cita iz kesa ako je podatak u kesu i ovo je ujedno idle stanje
		when IDLE =>
				if(tag(to_integer(unsigned(address(5 downto 2))))= address(31 downto 6) and 
					valid(to_integer(unsigned(address(5 downto 2)))) = '1') or
					(reset = '1')then
					addr := address(5 downto 2) & num_of_words(1 downto 0);
					data_out <= data(to_integer(unsigned(addr(5 downto 0))));
					next_s<=IDLE;
					mem_data<=  "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ";
				
				else
					req<='1';
					if(ack = '1') then
					--STALL signal oznacava rad sa memorijom
						wait_mem <='1';
						mem_read <= '1';
						mem_addr<= (address(31 downto 6) & address(5 downto 2) & B"00") ; 
						mem_data<=  "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ";
						next_s<=READ_MEM;
					else
						mem_data <= "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ";
						next_s <= IDLE;
					end if;
				end if;
		
		when READ_MEM =>
					req<='1';
					wait_mem <='1';
					mem_read <= '1';
					mem_addr<= (address(31 downto 6) & address(5 downto 2) & num_of_words ) ; 
					mem_data<=  "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ";
					if(num_of_words=B"11") then
						next_s <= WAITING;
					else
						next_s <= READ_MEM;
					end if;
		
		when WAITING =>
					req<='0';
					wait_mem <='1';
					mem_data<=  "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ";
					if(mem_wait=B"00") then
						next_s <= READ_DATA_FROM_BUS;
					else
						next_s <= WAITING;
					end if;
		
					
		when READ_DATA_FROM_BUS =>
					req<='1';
					wait_mem <='1';
					mem_data<=  "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ";
					if(num_of_words=B"11") then
						--ipak mora jos 1 takt da se saceka sa otkazivanjem zahteva da ne bi i data kes uskocio
						req <= '1';
						next_s <= IDLE;
					else
						next_s <= READ_DATA_FROM_BUS;
					end if;
		end case;


end process;
end behavior;