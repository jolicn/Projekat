library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;

entity Data_Mem is
port
	(
		address: in std_logic_vector(31 downto 0); -- adresa u kesu
	   data_in: in std_logic_vector(31 downto 0); --podatak koji trebamo da upisemo u data
		data_out: out std_logic_vector(31 downto 0); --procitani podatak
		
		reset	: in  std_logic; 
		clk	: in  std_logic;
		
			
		rd: in std_logic;
		wr: in std_logic;
		stop: in std_logic;
		
		stall: out std_logic;
		-- kada kes radi sa memorijom
		
		wait_mem: out std_logic;
	
		-- ABUS and DBUS
		mem_addr: out std_logic_vector(31 downto 0);
		mem_data: inout std_logic_vector(31 downto 0);
		
		-- memory read i write signal
	   mem_read: out std_logic;
		mem_write: out std_logic;
	
		-- request and acknowledgement for MEM access
		req: out std_logic;
		ack: in std_logic
		
	);
end entity;


architecture behavior of Data_Mem is

type data_type is array (2**6-1 downto 0) of std_logic_vector(31 downto 0);
type tag_type is array (2**4-1 downto 0) of std_logic_vector(25 downto 0);

signal valid: std_logic_vector(15 downto 0);
signal dirty: std_logic_vector(15 downto 0);

signal data: data_type;
signal tag: tag_type;

type cache_state is(IDLE, FLUSHING, READ_MEM, WAITING_AFTER_FLUSH, WAITING, READ_DATA_FROM_BUS);
signal current_s, next_s: cache_state;

--broj reci
signal num_of_words: std_logic_vector(1 downto 0);

--broji koliko memorija treba da ceka
signal mem_waiting_time: std_logic_vector(3 downto 0);

--sledeci ulaz za izbacivanje
signal entry: std_logic_vector(3 downto 0);

--signali za sinhronizaciju
signal in_cashe: std_logic;
signal writing, reading: std_logic;
signal tmpAddress: std_logic_vector(5 downto 0); 
signal flush: std_logic;
signal tmpRead: std_logic;
signal tmpWrite: std_logic;

begin

process(clk,reset) is
variable addr: std_logic_vector (5 downto 0);
begin


		
				if (reset = '1') then 
			
				current_s <= IDLE;

				--inicijalizacija valid i dirty bita
				for i in 0 to 15 loop
				
					valid(i) <= '0';
					dirty(i) <= '0';
					
				end loop;
				
				data_out <= "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ";
				mem_addr <= "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ";
				mem_data <= "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ";
				mem_read <= 'Z';
				mem_write <= 'Z';
				wait_mem <= '0';
				stall <= '0';
				reading <= '0';
				writing <= '0';
				tmpRead <= '0';
				tmpWrite <= '0';
				tmpAddress <= "ZZZZZZ";
				
				
				else if(rising_edge(clk)) then
					
					data_out <= "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ";
					mem_addr <= "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ";
					mem_data <= "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ";
					mem_read <= 'Z';
					mem_write <= 'Z';
					wait_mem <= '0';
					stall <= '0';
					reading <= '0';
					writing <= '0';
					tmpAddress <= "ZZZZZZ";
					tmpRead <= '0';
				   tmpWrite <= '0';
					
					
					if(current_s = IDLE) then
					
						if(next_s = FLUSHING) then
						
							num_of_words<= B"01"; --sledeca rec za izbacivanje
							if(flush = '1') then --ako je stop instrukcija u pitanju radi se FLUSH svih zaprljanih ulaza ako ih ima
								addr := entry & B"00";
							else  -- ako je ulaz validan i zaprljan prelazi se u stanje FLUSHING gde se ulaz upisuje opet u memoriju
								addr := address(5 downto 2) & B"00";
							end if;
							--tmp jer ovde treba da se cita iz kesa
							tmpAddress <= addr;
							reading <= '1';
							
						elsif(next_s = READ_MEM) then
						-- ako trazeni ulaz nije u memoriji a nije ni zaprljan onda se jednostavno dovlaci iz memorije ceo blok	
							num_of_words <= B"01";
							mem_addr <= address(31 downto 2) & B"00";
							mem_read<= '1';
							
						--next_s je IDLE
						else
							
							num_of_words <= B"00";
							
							if(in_cashe = '1') then
								if(rd = '1') then
									reading <= '1';
									tmpAddress <= address(5 downto 0);
									
								elsif (wr = '1') then
									writing <= '1';
									tmpAddress <= address(5 downto 0);
									dirty(to_integer(unsigned(address(5 downto 2)))) <= '1';
								end if;
								
								
							else
								null;
							end if;
						
						
						end if;
				elsif (current_s = FLUSHING) then
						if(flush = '0') then
							addr := address(5 downto 2) & num_of_words;
						else
							addr := entry & num_of_words;
						end if;
						tmpWrite <= '1';
						tmpAddress <= addr;
						if(next_s = WAITING_AFTER_FLUSH) then
							
							num_of_words <= B"00";
							mem_waiting_time <= X"D";
							--kad zavrsimo sa svim ulazima brisemo valid i dirty bite
							if(flush= '0') then
								valid(conv_integer(address(5 downto 2))) <= '0';
								dirty(conv_integer(address(5 downto 2))) <= '0';
							else
								valid(conv_integer(entry)) <= '0';
								dirty(conv_integer(entry)) <= '0';
							end if;
							
						else

							num_of_words <= num_of_words + 1;
							
						end if;
					

		
			elsif (current_s = READ_MEM) then
						
						mem_addr <= address(31 downto 2) & num_of_words;
						mem_read <= '1';
						
						if(next_s = WAITING) then
							
							num_of_words <= B"00";
							mem_waiting_time <= X"D";
									
						else
							
							num_of_words <= num_of_words + 1;
							
						end if;
					
	
		   elsif (current_s = WAITING_AFTER_FLUSH) then
						if(next_s = IDLE ) then
							
							mem_waiting_time<= X"0";
							num_of_words <= B"00";
							
						else
							
							mem_waiting_time<= mem_waiting_time - 1;
							num_of_words <= B"00";
						
						end if;
					
			
		elsif (current_s = WAITING) then
						if(next_s = READ_DATA_FROM_BUS ) then
							
							mem_waiting_time<= X"0";
							num_of_words <= B"00";
							
						else
							
							mem_waiting_time<= mem_waiting_time - 1;
							num_of_words <= B"00";
						
						end if;
					
		
		elsif (current_s = READ_DATA_FROM_BUS) then
						if(next_s = IDLE ) then
							
							writing <= '1';
							tmpAddress <= address(5 downto 2) & num_of_words;
							tag(to_integer(unsigned(address(5 downto 2)))) <= address(31 downto 6);
							valid(to_integer(unsigned(address(5 downto 2)))) <= '1';
							
							
						else
							
							num_of_words <= num_of_words + 1;
							tmpAddress <= address(5 downto 2) & num_of_words;
							writing <= '1';
							
						end if;
					
		
	
		end if;
	
		
		current_s <= next_s;
		
		if(reading = '1' and writing = '0') then
			if(tmpWrite = '1') then --citanje iz kesa i upis u memoriju
				mem_addr <= tag(conv_integer(tmpAddress(5 downto 2))) & tmpAddress;
				mem_data <= data(conv_integer(tmpAddress));
				mem_write <= '1';
			else
				data_out <= data(conv_integer(tmpAddress));
			end if;
		else if(writing = '1' and reading = '0') then
			if(tmpRead = '1') then --citanje iz memorije i upis u kes
				data(conv_integer(tmpAddress)) <= mem_data;
			else
				data(conv_integer(tmpAddress)) <= data_in;
			end if;
		
		end if;
		
		end if;
		
		
		end if;
		
		end if;
				
					
				
end process;

process (address, data_in, reset,clk,rd,wr,stop,mem_data,ack,tag,valid,dirty,current_s,mem_waiting_time,num_of_words) is

variable addr: std_logic_vector (5 downto 0);
--ulaz za izbacivanje u slucaju stop instrukcije
variable entry_var: std_logic_vector(3 downto 0);

begin

next_s <= IDLE;
req  <= '0';
flush <= '0';
in_cashe <= '0';
entry <= B"0000";
		-- uncommented, because Quartus is generating latches for req



	case current_s is
				-- u ovom stanju se proverava dal je podatak u kesu i ovo je ujedno idle stanje
				when IDLE =>
				
						req<= '0';
				
				if(rd='1' or wr='1') then
					--ako se data nalazi u kesu
					if(tag(conv_integer(address(5 downto 2)))=address(31 downto 6)) 
						and (valid(conv_integer(address(5 downto 2)))='1') then
						
						in_cashe <= '1';
						next_s <= IDLE;
					--data se ne nalazi u kesu
					else
					
						req <= '1'; --posalji zahtev za pristup memoriji
						
						if(ack = '1') then --dobili smo potvrdan odgovor
							--ako je podatak validan i zaprljan pre izbacivanja zapisati u mem
							if (valid(conv_integer(address(5 downto 2)))='1' ) and (dirty(conv_integer(address(5 downto 2)))='1') then
								flush<= '0';
								next_s <= FLUSHING;
								
							else							
								
								next_s <= READ_MEM;
								
							end if;
						else
							in_cashe <= '0';
							next_s <= IDLE;
							
						end if;
						
					end if;
				else if (stop = '1')	then
					
					req<= '1';
					
						if(ack = '1') then
						
							if(std_match(dirty,(X"00"&X"00") )) then
	
								next_s<=IDLE;
							else
								entry_var := B"0000";
								for i in 0 to 3 loop	
									if((valid(i) = '1') and (dirty(i) = '1')) then
										
										entry_var := std_logic_vector(to_unsigned(i, 4));
										
									end if;
								end loop;							
								
								entry <= entry_var;
								flush<= '1';
								next_s <= FLUSHING;
							
							end if;
					end if;
				else
					
					next_s<= IDLE;
					end if;
				end if;
				
				--ovde saljemo zahteve za upis u memoriju
				
				when FLUSHING => 
				
					req <='1';
					
					if(num_of_words=B"11") then
						next_s <= WAITING_AFTER_FLUSH;
					else
						next_s <= FLUSHING;
					end if;
				
				when READ_MEM => --saljemo zahteve za citanje podataka iz memorije
				
					req <='1';
					
					if(num_of_words=B"11") then
						next_s <= WAITING;
					else
						next_s <= READ_MEM;
					end if;
				
				when WAITING_AFTER_FLUSH => --ovde cekamo da sve zahteve koje smo poslali memoriji za upis ona obradi
					
					req<='1';
				
					if(mem_waiting_time=B"0000") then
					
						next_s<= IDLE;
						
						req<='1';
						
					else
						next_s <= WAITING_AFTER_FLUSH;
					end if;
				
				when WAITING =>
				
					req<='1';
				
					if(mem_waiting_time=B"0000") then
						next_s<= READ_DATA_FROM_BUS;
						req<='1';
					else
						next_s <= WAITING;
					end if;
				
				when READ_DATA_FROM_BUS =>
				
					req<='1';
				
					if(num_of_words=B"11") then
						next_s<= IDLE;
						req<='1';
					else
						next_s <= READ_DATA_FROM_BUS;
					end if;
				
				
end case;						
				
end process;

end behavior;