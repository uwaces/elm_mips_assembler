# MIPS Assembler
By: Harrison Kaiser and Chris Phifer

For our final project, we implemented a MIPS assembler using the Elm programming
language (a compile-to-JavaScript ML-like language with a strong static type
system).

To use the assembler, simply open index.html in your favorite web browser.
Some sample programs can be loaded using the buttons on the page.

To transform the output machine code into VHDL-ready initialization:

1. Copy the ASCII machine code into a text file named program.txt
2. Run:
   ```bash
   python gen_program.py > output
   ```
   On the command line
3. Copy the output into the appropriate VHDL file for initializing program
   memory. For an example, see program.txt and gen_program_example.txt.

