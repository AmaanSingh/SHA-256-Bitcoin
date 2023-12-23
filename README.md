# SHA-256
Implemented SHA-256. SHA-256 is a encryption algorithm that takes up to 2^64 bit of “message” and generates a 256-bit output. Breaks the “message” into block size of 512 bit (16 words). The last block will contain the length of the message in the last 64 bits. Each message will generate an almost guaranteed unique hash code output.

# Bitcoin 
Implemented Bitcoin hashing using the previously built SHA-256. A simplified version of bitcoin blockchain hashing with 16 nonce (unique seeds).
