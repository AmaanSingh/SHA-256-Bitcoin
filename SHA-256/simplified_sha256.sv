module simplified_sha256 #(parameter integer NUM_OF_WORDS = 40)(
 input logic  clk, rst_n, start,
 input logic  [15:0] input_addr, hash_addr,
 output logic done, memory_clk, memory_we,
 output logic [15:0] memory_addr,
 output logic [31:0] memory_write_data,
 input logic [31:0] memory_read_data);

// FSM state variables 
enum logic [2:0] {IDLE, BLOCK, COMPUTE, WRITE} state;

parameter integer SIZE = NUM_OF_WORDS; 

// NOTE : Below mentioned frame work is for reference purpose.
// Local variables might not be complete and you might have to add more variables
// or modify these variables. Code below is more as a reference.

// Local variables // might need to add dsparm[]
logic [31:0] w[64];
logic [31:0] S0,S1;
logic [31:0] hash0, hash1, hash2, hash3, hash4, hash5, hash6, hash7;
logic [31:0] A, B, C, D, E, F, G, H;
logic [ 7:0] i, j;
logic [15:0] offset; // in word address
logic [ 7:0] num_blocks;
logic        enable_write;
logic [15:0] present_addr;
logic [31:0] present_write_data;
logic [512:0] data_read;
logic [10:0] word_left;
logic [10:0] word_leftover;
logic [15:0] update;
logic [3:0] change_mode;
logic [63:0] message_len;
logic [31:0] message_len1;
logic [31:0] message_len2;



// SHA256 K constants
parameter int k[0:63] = '{
   32'h428a2f98,32'h71374491,32'hb5c0fbcf,32'he9b5dba5,32'h3956c25b,32'h59f111f1,32'h923f82a4,32'hab1c5ed5,
   32'hd807aa98,32'h12835b01,32'h243185be,32'h550c7dc3,32'h72be5d74,32'h80deb1fe,32'h9bdc06a7,32'hc19bf174,
   32'he49b69c1,32'hefbe4786,32'h0fc19dc6,32'h240ca1cc,32'h2de92c6f,32'h4a7484aa,32'h5cb0a9dc,32'h76f988da,
   32'h983e5152,32'ha831c66d,32'hb00327c8,32'hbf597fc7,32'hc6e00bf3,32'hd5a79147,32'h06ca6351,32'h14292967,
   32'h27b70a85,32'h2e1b2138,32'h4d2c6dfc,32'h53380d13,32'h650a7354,32'h766a0abb,32'h81c2c92e,32'h92722c85,
   32'ha2bfe8a1,32'ha81a664b,32'hc24b8b70,32'hc76c51a3,32'hd192e819,32'hd6990624,32'hf40e3585,32'h106aa070,
   32'h19a4c116,32'h1e376c08,32'h2748774c,32'h34b0bcb5,32'h391c0cb3,32'h4ed8aa4a,32'h5b9cca4f,32'h682e6ff3,
   32'h748f82ee,32'h78a5636f,32'h84c87814,32'h8cc70208,32'h90befffa,32'ha4506ceb,32'hbef9a3f7,32'hc67178f2
};


// Generate request to memory
// for reading from memory to get original message
// for writing final computed has value
assign memory_clk = clk;
assign memory_addr = present_addr + offset + j*16;
assign memory_we = enable_write;
assign memory_write_data = present_write_data;

assign num_blocks = determine_num_blocks(NUM_OF_WORDS);
//assign word_left = 14 - (40 - ((num_blocks-1) * 16));
//assign word_leftover = (40 - ((num_blocks-1) * 16));



//assign tstep = (i - 1);

// Note : Function defined are for reference purpose. Feel free to add more functions or modify below.
// Function to determine number of blocks in memory to fetch
function logic [15:0] determine_num_blocks(input logic [31:0] size);

  // Student to add function implementation
  logic [31:0] holder, divider, blocks;

  
  holder = size*32;
  blocks = 1;
  divider = 447;
  while(divider <= holder) begin
    if(divider + 512 <= holder)begin
        divider = divider + 512;
        blocks = blocks + 1;
    end else begin
      divider = divider + 512;
      blocks = blocks + 1;
    end
  end
  determine_num_blocks = blocks;
 
endfunction


// SHA256 hash round
function logic [255:0] sha256_op(input logic [31:0] a, b, c, d, e, f, g, h, w,
                                 input logic [7:0] t);
    logic [31:0] S1, S0, ch, maj, t1, t2; // internal signals
begin
    S1 = ror(e, 6) ^ ror(e, 11) ^ ror(e, 25);
    ch = (e & f) ^ (~e & g);
    t1 = h + S1 + ch + k[t] + w;
    S0 = ror(a, 2) ^ ror(a, 13) ^ ror(a, 22);
    maj = (a & b) ^ (a & c) ^ (b & c);
    t2 = S0 + maj;

    sha256_op = {t1 + t2, a, b, c, d + t1, e, f, g};
end
endfunction

//Word Expansion
function logic [31:0] wordExpand (input logic[7:0] t);
				logic [31:0] s0,s1;
				s0 = ror(w[t-15], 7) ^ ror(w[t-15], 18) ^ (w[t-15] >> 3);
				s1 = ror(w[t-2], 17) ^ ror(w[t-2], 19) ^ (w[t-2] >> 10);
				wordExpand = w[t-16] + s0 + w[t-7] + s1;
endfunction

// Right Rotation Example : right rotate input x by r
// Lets say input x = 1111 ffff 2222 3333 4444 6666 7777 8888
// lets say r = 4
// x >> r  will result in : 0000 1111 ffff 2222 3333 4444 6666 7777 
// x << (32-r) will result in : 8888 0000 0000 0000 0000 0000 0000 0000
// final right rotate expression is = (x >> r) | (x << (32-r));
// (0000 1111 ffff 2222 3333 4444 6666 7777) | (8888 0000 0000 0000 0000 0000 0000 0000)
// final value after right rotate = 8888 1111 ffff 2222 3333 4444 6666 7777
// Right rotation function

function logic [31:0] ror(input logic [31:0] in,input logic [7:0] s);
								  
begin
	ror = (in >> s) | (in << (32 - s));
end
endfunction


always_ff @(posedge clk, negedge rst_n)
begin
  if (!rst_n) begin
    state <= IDLE;
  end 
  else begin
      //$display("SHA_256_Started");
	  //$display("Word_leftover: %d", word_left);
      $display("current_state: %0s", state);
      case (state)
		// Set the initial state for the FSM
		// Initialize hash values h0 to h7 and a to h, other variables and memory we, address offset, etc
		IDLE: begin
			//$display("IDLE_Start"); 
			if(start) begin
			  hash0 <= 32'h6a09e667;
			  hash1 <= 32'hbb67ae85;
			  hash2 <= 32'h3c6ef372;
			  hash3 <= 32'ha54ff53a;
			  hash4 <= 32'h510e527f;
			  hash5 <= 32'h9b05688c;
			  hash6 <= 32'h1f83d9ab;
			  hash7 <= 32'h5be0cd19;
	
			  // Initialize other variables
			  A <= 32'h6a09e667;
			  B <= 32'hbb67ae85;
			  C <= 32'h3c6ef372;
			  D <= 32'ha54ff53a;
			  E <= 32'h510e527f;
			  F <= 32'h9b05688c;
			  G <= 32'h1f83d9ab;
			  H <= 32'h5be0cd19;
			  i <= 0;
			  j <= 0;

			  word_left <= 14 - (SIZE - ((num_blocks-1) * 16));
			  //word_leftover <= (SIZE - ((num_blocks-1) * 16));
			  if(SIZE < (num_blocks-1) * 16) begin
				change_mode = 1;
				word_leftover <= 0;
				word_left <= (16 - (((num_blocks-1) * 16) - SIZE));
				//word_left <= 16;
				//word_left <= word_left - 1;
			  end else begin
				word_leftover <= (SIZE - ((num_blocks-1) * 16));
				change_mode = 0;
			  end
			  //$display("num_blocks: %d", num_blocks);
			  message_len = SIZE*32;
			  message_len1 = message_len[63:32];
			  message_len2 = message_len[31:0];
			  update = 1;
			  offset <= 0;
			  enable_write <= 0;
			  present_addr <= input_addr;
			  $display("present_address: %b", present_addr);
			  $display("hash_addr: %b", hash_addr);
			  present_write_data <= 32'h00000000;
			  data_read <= 0;
			  state <= BLOCK;
			 end
			 else begin
				state <= IDLE;
			 end		  
		end
	
	// SHA-256 FSM 
	// Get a BLOCK from the memory, COMPUTE Hash output using SHA256 function    
	// and write back hash value back to memory
	// Fetch message in 512-bit block size
	// For each of 512-bit block initiate hash value computation
		BLOCK: begin
			 
			 //$display("Num_blocks: %d", num_blocks);
			 
			 //decides if extra block for padding is needed
			 //One extra block needed, padding starts from the second to last block
			 /*
			 if(SIZE % 16 > 13) begin
			 
		
			//Change
			 
			 
					if (j == num_blocks - 2) begin 
						state <= BLOCK;
					end
					else begin
						state <= BLOCK;
					end
			 end
			 //One extra block needed, padding starts from the second to last block
			 else if (SIZE % 16 == 0) begin
			 
			 
			 //Change
			 
			 
				   state <= BLOCK;
						
		
			 end
			 */
			 //No extra padding block or intermedite blocks
			 //else begin
				 //block 0 to N-1
		// did normal padding for now haven't done edge cases
				 if (j < num_blocks) begin
					 //Waits 1 cycle for memory_read to read the data
					 if (offset <= 0) begin
						offset <= offset + 1;
					 end
					 else if (offset >= 1 && offset < 17) begin
						 $display("input_addr: %b", input_addr);
						 $display("mem_addr: %b", memory_addr);
						 $display("read_data: %h", memory_read_data);
						 $display("offset: %d", offset);
						 $display("word_leftover: %d", word_leftover);
						 $display("word_left: %d", word_left);
						 $display("num_blocks: %d", num_blocks);    
						 w[offset-1]<= memory_read_data; 
						 //checks the leftover padding by checking how much remainder word
						 if(change_mode == 1 && j == num_blocks - 2 && offset-1 == word_left) begin
							update <= update + 1;
							if(update == 1) begin
								w[offset-1] <= 32'b10000000000000000000000000000000;
								word_left <= word_left + 1;
							end else begin
								w[offset-1] <= 32'b0;
								word_left <= word_left + 1;
							end
							if(offset == 16) begin
								update <= update + 0;
								change_mode <= 0;
							end
						 end
						 if(j == num_blocks - 1 && offset-1 == word_leftover && offset-1 != 14) begin
							if(word_leftover < 14) begin
								word_leftover <= word_leftover + 1;
								update <= update + 1;
							end
							if(update == 1 && change_mode == 0) begin
								w[offset-1] <= 32'b10000000000000000000000000000000;
								//$display("w[offset-1]: %b", 32'b10000000000000000000000000000000); 
							end
							else begin
								w[offset-1] <= 32'b0;
								//$display("message_len2: %d", message_len2); 
						
							end
						 end
						 //adds message length to w[14] and w[15]
						 if(j == num_blocks - 1) begin
						 	if(offset-1 == 14 || offset-1 == 15) begin
								if(offset-1 == 14) begin
									w[offset-1] <= message_len1;
								end else begin
									w[offset-1] <= message_len2;
									$display("message_len2: %d", message_len2); 
								end
						 	end
						 end
						 offset <= offset + 1;
						 state <= BLOCK;			 
					 end 
					 else begin 
						offset <= 0;
						state <= COMPUTE;
					 end
				 end
				 //Last block
				 else begin

				 //Change
					   state <= WRITE;
				 end
			// end
		end
			
		// For each block compute hash function
		// Go back to BLOCK stage after each block hash computation is completed and if
		// there are still number of message blocks available in memory otherwise
		// move to WRITE stage
		COMPUTE: begin
		// 64 processing rounds steps for 512-bit block
		  //$display("Comp_state_begin");
			if (i < 64) begin
			  if (i < 48) begin
					w[i+16] <= wordExpand(i+16);
			  end
			  // Compute SHA-256 operations for each round
			  {A, B, C, D, E, F, G, H} <= sha256_op(A, B, C, D, E, F, G, H, w[i], i);
			  // Update variables and move to the next round
			  i <= i + 1;
			end 
			else begin
			  // Finished all rounds for this block, move back to BLOCK state
			  hash0 <= A + hash0;
			  hash1 <= B + hash1;
			  hash2 <= C + hash2;
			  hash3 <= D + hash3;
			  hash4 <= E + hash4;
			  hash5 <= F + hash5;
			  hash6 <= G + hash6;
			  hash7 <= H + hash7;
			  A <= A + hash0;
			  B <= B + hash1;
			  C <= C + hash2;
			  D <= D + hash3;
			  E <= E + hash4;
			  F <= F + hash5;
			  G <= G + hash6;
			  H <= H + hash7;
			  j <= j+1;
			  if (j < num_blocks) begin
			      i <= 0;
					state <= BLOCK;
			  end
			  else begin
			  		i <= 0;
					state <= WRITE;
			  end
		  end	
		end
	
		// h0 to h7 each are 32 bit hashes, which makes up total 256 bit value
		// h0 to h7 after compute stage has final computed hash value
		// write back these h0 to h7 to memory starting from output_addr
		WRITE: begin
		//need 8 cycles to write h0 - h7 into memory
			j <= 0;
			case (i)
			0 : begin
				enable_write <= 1'b1;
			   present_addr <= hash_addr;
				offset <= i;
			   present_write_data <= hash0;
				i <= i +1;
			end
			1 : begin
			   enable_write <= 1'b1;
			   present_addr <= hash_addr;
				offset <= i;
			   present_write_data <= hash1;
				i <= i +1;
			end
			2 : begin
				enable_write <= 1'b1;
			   present_addr <= hash_addr;
				offset <= i;
			   present_write_data <= hash2;
				i <= i + 1;
			end
			3 : begin
				enable_write <= 1'b1;
			   present_addr <= hash_addr;
				offset <= i;
			   present_write_data <= hash3;
				i <= i +1;
			end
			4 : begin
				enable_write <= 1'b1;
			   present_addr <= hash_addr;
				offset <= i;
			   present_write_data <= hash4;
				i <= i +1;
			end
			5 : begin
				enable_write <= 1'b1;
			   present_addr <= hash_addr;
				offset <= i;
			   present_write_data <= hash5;
				i <= i +1;
			end
			6 : begin
				enable_write <= 1'b1;
			   present_addr <= hash_addr;
				offset <= i;
			   present_write_data <= hash6;
				i <= i +1;
			end
			7 : begin
				enable_write <= 1'b1;
			   present_addr <= hash_addr;
				offset <= i;
			   present_write_data <= hash7;
			   i <= i +1;
			   
			end			  
			default : begin
				enable_write <= 1'b0;
				offset <= 0;
				state <= IDLE;
			end
	      endcase		
		end
		endcase  
	 end
end

// Generate done when SHA256 hash computation has finished and moved to IDLE state
assign done = (state == IDLE);

endmodule