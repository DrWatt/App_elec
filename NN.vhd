LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.math_real.ALL;
USE work.float_pkg.ALL;
--------------------------- PACKAGE WITH TYPE DEFINITIONS AND EXPONENTIAL AND TANH DEFINITIONS
PACKAGE typpes IS
    TYPE FLOAT_VECTOR IS ARRAY (NATURAL RANGE <>) OF FLOAT32;
    TYPE matrix IS ARRAY (NATURAL RANGE <>) OF FLOAT_VECTOR;
    TYPE posint_array IS ARRAY (NATURAL RANGE <>) OF POSITIVE; 
    FUNCTION tanh (SIGNAL s: FLOAT32) RETURN FLOAT32;
    FUNCTION exp2 (SIGNAL s: FLOAT32) RETURN FLOAT32;
    FUNCTION dot_prod (SIGNAL a: FLOAT_VECTOR;SIGNAL b: FLOAT_VECTOR) RETURN FLOAT32;
    FUNCTION subtrac (a: FLOAT_VECTOR;SIGNAL b: FLOAT_VECTOR) RETURN FLOAT_VECTOR;
END PACKAGE;

PACKAGE BODY typpes IS
--------------------------- EXPONENTIAL EXP(2*X)
    FUNCTION exp2 (SIGNAL s: FLOAT32) RETURN FLOAT32 IS
        VARIABLE e: FLOAT32 := "01000000001011011111100001010100";
        VARIABLE sp: FLOAT32:= TO_FLOAT32(2*s);
        VARIABLE int: INTEGER := TO_INTEGER(sp);
        VARIABLE re: FLOAT32 := sp - TO_FLOAT(int);
        VARIABLE temp: FLOAT32:= TO_FLOAT(1);
        VARIABLE temp2: FLOAT32:= TO_FLOAT(1);
        BEGIN
            FOR i IN 0 TO int-1 LOOP
                temp:= temp*e;
            END LOOP;
            
            temp2 := TO_FLOAT(1)+re+(re*re)/TO_FLOAT(2)+(re*re*re)/TO_FLOAT(6)+(re*re*re*re)/TO_FLOAT(24)+(re*re*re*re*re)/TO_FLOAT(120);
            
        RETURN temp*temp2;
        
    END FUNCTION;
    -------------------HYPERBOLIC TANGENT
    FUNCTION tanh (SIGNAL s: FLOAT32) RETURN FLOAT32 IS
        VARIABLE temp2: FLOAT32;
        VARIABLE temp3: FLOAT32;
        VARIABLE p0: FLOAT32 := TO_FLOAT(-0.8237728127);
        VARIABLE p1: FLOAT32 := TO_FLOAT(-0.003831010665);
        VARIABLE q0: FLOAT32 := TO_FLOAT(2.471319654);
        VARIABLE g: FLOAT32;
        VARIABLE pp: FLOAT32;
        VARIABLE qq: FLOAT32;
        VARIABLE r: FLOAT32;
        BEGIN
            IF s < "00111001110111011011001111010111" THEN
                RETURN s;
            ELSIF s < "00111111000011001001111101010100" THEN
                g:= s*s;
                pp:= p0 + p1*g;
                qq := q0 + g;
                r:= (s*pp)/qq;
                
                RETURN s + s*r; 
            
            ELSIF s < "01000001000010101010000100100011" THEN
                temp3 := TO_FLOAT(1)+exp2(s);
                temp2:= TO_FLOAT(0.5) - TO_FLOAT(1)/temp3;
                RETURN temp2+temp2; 
            
            ELSE
                RETURN TO_FLOAT(1);
            
            END IF;
    END FUNCTION;
    -------------- DOT PRODUCT
    FUNCTION dot_prod (SIGNAL a: FLOAT_VECTOR;SIGNAL b: FLOAT_VECTOR) RETURN FLOAT32 IS
        VARIABLE temp: FLOAT32 := "00000000000000000000000000000000";
        BEGIN
            IF a'LENGTH > b'LENGTH THEN
                FOR i IN b'RANGE LOOP
                    temp := temp + a(i)*b(i);
                END LOOP;
            ELSE
                FOR i IN a'RANGE LOOP
                    temp := temp + a(i)*b(i);
                END LOOP;
            END IF;
            --temp:= TO_FLOAT32(TO_FLOAT(TANH(TO_REAL(temp))));
        RETURN temp;
    END FUNCTION;
    
    FUNCTION subtrac (a: FLOAT_VECTOR;SIGNAL b: FLOAT_VECTOR) RETURN FLOAT_VECTOR IS
    VARIABLE temp: FLOAT_VECTOR(a'RANGE);
    VARIABLE lendiff: INTEGER;
    BEGIN
        IF a'LENGTH > b'LENGTH THEN
            lendiff := a'LENGTH - b'LENGTH;
            FOR i in b'RANGE LOOP
                temp(i) := a(i) - b(i);
            END LOOP;
            FOR i IN lendiff - 1 DOWNTO 0 LOOP
                temp(i) := a(i);
            END LOOP;
        ELSE 
            FOR i in a'RANGE LOOP
                temp(i) := a(i) - b(i);
            END LOOP;
        END IF;
        RETURN temp;
    END FUNCTION;

END typpes;
-------------------------------------------------
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE work.typpes.ALL;
USE work.float_pkg.ALL;

ENTITY nn IS
    GENERIC(
        N : POSITIVE := 3;
    	M: POSITIVE := 8;
    	--n_layer : STD_LOGIC_VECTOR(7 DOWNTO 0) := "00000010";
    	n_nodes_layer : posint_array(0 TO 100):= (2,3,5,1,OTHERS=>1)
    	);
    PORT(
    	input_data : IN FLOAT_VECTOR(M-1 DOWNTO 0);
    	--input_data : IN FLOAT32;
    	output:	OUT FLOAT32
    	);
END ENTITY;
-----------------------------------------------------
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.math_real.ALL;
USE work.typpes.ALL;
USE work.float_pkg.ALL;
USE work.fixed_pkg.ALL;

ENTITY layer IS
    GENERIC (
        in_dim,nodes: POSITIVE;
        N: POSITIVE := 3);
    PORT(
     	input_layer: IN FLOAT_VECTOR(in_dim -1 DOWNTO 0);
     	weights: IN matrix(nodes-1 DOWNTO 0)(in_dim-1 DOWNTO 0);
     	bias: IN FLOAT32;
        output_layer: OUT FLOAT_VECTOR(nodes -1 DOWNTO 0)
    	);
END ENTITY;
------------------------------------------------------
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.math_real.ALL;
USE work.typpes.ALL;
USE work.float_pkg.ALL;
USE work.fixed_pkg.ALL;

ENTITY out_pred IS
    GENERIC (
        in_dim: POSITIVE;
        N: POSITIVE := 3);
    PORT(
     	input_layer: IN FLOAT_VECTOR(in_dim -1 DOWNTO 0);
     	weights: IN matrix(0 DOWNTO 0)(in_dim-1 DOWNTO 0);
     	bias: IN FLOAT32;
        output_pred: OUT FLOAT32
    	);
END ENTITY;
---------------------------------
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.math_real.ALL;
USE work.typpes.ALL;
USE work.float_pkg.ALL;
USE work.fixed_pkg.ALL;
ENTITY out_back IS
    GENERIC (
        N: POSITIVE := 3);
    PORT(
        input_pred: IN FLOAT32;
        input_true: IN FLOAT_VECTOR(N-1 DOWNTO 0);
        output_back: OUT FLOAT_VECTOR(N-1 DOWNTO 0);
        clk: IN BIT
        );
END ENTITY;
--------------------------------------------------
ARCHITECTURE lay_behavioural OF layer IS
    SIGNAL out_temp: FLOAT_VECTOR(nodes -1 DOWNTO 0);
    BEGIN
        gen: FOR i IN output_layer'RANGE GENERATE
            out_temp(i) <= dot_prod(input_layer,weights(i)) + bias;
            output_layer(i) <= tanh(out_temp(i));
        END GENERATE;
END ARCHITECTURE;
    

ARCHITECTURE layout_behavioural OF out_pred IS
BEGIN
    output_pred <= dot_prod(input_layer,weights(0)) + bias;
END ARCHITECTURE;

ARCHITECTURE start_back OF out_back IS
BEGIN
    PROCESS(input_pred,clk)
    VARIABLE bufferr: FLOAT_VECTOR(N-1 DOWNTO 0);
    VARIABLE cap: INTEGER := 0;
    BEGIN
        FOR i IN N-1 DOWNTO 0 LOOP
            IF i = N-1 THEN
                bufferr(i) := input_pred;
            ELSIF (bufferr(i+1) /= input_pred) THEN
                bufferr(i) := input_pred;
            END IF;

        --cap := cap + 1;
        --IF cap = 10 THEN count:= N; END IF;
        END LOOP;
        output_back <= subtrac(bufferr,input_true);
    END PROCESS;

END ARCHITECTURE;

------------------------------------------------
ARCHITECTURE forward_nn OF nn IS
    COMPONENT layer IS
        GENERIC(in_dim,nodes: POSITIVE);
        PORT(
            input_layer: IN FLOAT_VECTOR(in_dim -1 DOWNTO 0);
         	weights: IN matrix(nodes-1 DOWNTO 0)(in_dim-1 DOWNTO 0) := (OTHERS=>(OTHERS=>TO_FLOAT(1.0,8,23)));
            bias: IN FLOAT32 := "00000000000000000000000000000000";
            output_layer: OUT FLOAT_VECTOR(nodes -1 DOWNTO 0)
        	);
    END COMPONENT;

COMPONENT out_pred IS
    GENERIC(
        in_dim: POSITIVE;
        N: POSITIVE := 3);
    PORT(
     	input_layer: IN FLOAT_VECTOR(in_dim -1 DOWNTO 0);
     	weights: IN matrix(0 DOWNTO 0)(in_dim-1 DOWNTO 0) := (OTHERS=>(OTHERS=>TO_FLOAT(1.0,8,23)));
     	bias: IN FLOAT32 := "00000000000000000000000000000000";
        output_pred: OUT FLOAT32
    	);
END COMPONENT;
--SIGNAL buffer0: FLOAT_VECTOR(M-1 DOWNTO 0);
    SIGNAL buffer1: FLOAT_VECTOR(n_nodes_layer(0)-1 DOWNTO 0);
    SIGNAL buffer2: FLOAT_VECTOR(n_nodes_layer(1)-1 DOWNTO 0);
    SIGNAL buffer3: FLOAT_VECTOR(n_nodes_layer(2)-1 DOWNTO 0);

    BEGIN
    --layin: in_layer GENERIC MAP(M,N) PORT MAP(input_da=>input_data,output_layer=>buffer0);
    lay0: layer GENERIC MAP (M,n_nodes_layer(0)) PORT MAP(input_layer=>input_data, output_layer=>buffer1);
    lay1: layer GENERIC MAP (n_nodes_layer(0),n_nodes_layer(1)) PORT MAP(input_layer=>buffer1, output_layer=>buffer2);
    lay2: layer GENERIC MAP (n_nodes_layer(1),n_nodes_layer(2)) PORT MAP(input_layer=>buffer2, output_layer=>buffer3);
    lay3: out_pred GENERIC MAP (n_nodes_layer(2)) PORT MAP(input_layer=>buffer3, output_pred=>output);
END ARCHITECTURE;
-------------------------------------------------------------

ARCHITECTURE backward_nn OF nn IS
    COMPONENT layer IS
        GENERIC (in_dim,nodes: POSITIVE);
        PORT(
            input_layer: IN FLOAT_VECTOR(in_dim -1 DOWNTO 0);
         	weights: IN matrix(nodes-1 DOWNTO 0)(in_dim-1 DOWNTO 0) := (OTHERS=>(OTHERS=>TO_FLOAT(1.0,8,23)));
            bias: IN FLOAT32 := "00000000000000000000000000000000";
            output_layer: OUT FLOAT_VECTOR(nodes -1 DOWNTO 0)
        	);
    END COMPONENT;

    COMPONENT out_pred IS
        GENERIC(
            in_dim: POSITIVE;
            N: POSITIVE := 3);
        PORT(
         	input_layer: IN FLOAT_VECTOR(in_dim -1 DOWNTO 0);
         	weights: IN matrix(0 DOWNTO 0)(in_dim-1 DOWNTO 0) := (OTHERS=>(OTHERS=>TO_FLOAT(1.0,8,23)));
         	bias: IN FLOAT32 := "00000000000000000000000000000000";
            output_pred: OUT FLOAT32
    	   );
    END COMPONENT;

    COMPONENT out_back IS
        GENERIC (
            N: POSITIVE := 3);
        PORT(
            input_pred: IN FLOAT32;
            output_back: OUT FLOAT_VECTOR(N-1 DOWNTO 0);
            clk: IN BIT:= '0'
            );
    END COMPONENT;
--SIGNAL buffer0: FLOAT_VECTOR(M-1 DOWNTO 0);
    SIGNAL buffer1: FLOAT_VECTOR(n_nodes_layer(0)-1 DOWNTO 0);
    SIGNAL buffer2: FLOAT_VECTOR(n_nodes_layer(1)-1 DOWNTO 0);
    SIGNAL buffer3: FLOAT_VECTOR(n_nodes_layer(2)-1 DOWNTO 0);
    SIGNAL buffer4: FLOAT32;
    SIGNAL buffer5: FLOAT_VECTOR(N-1 DOWNTO 0);
    SIGNAL D: FLOAT_VECTOR(N-1 DOWNTO 0) := (OTHERS=>TO_FLOAT(1.0,8,23));

BEGIN
    lay0: layer GENERIC MAP (M,n_nodes_layer(0)) PORT MAP(input_layer=>input_data, output_layer=>buffer1);
    lay1: layer GENERIC MAP (n_nodes_layer(0),n_nodes_layer(1)) PORT MAP(input_layer=>buffer1, output_layer=>buffer2);
    lay2: layer GENERIC MAP (n_nodes_layer(1),n_nodes_layer(2)) PORT MAP(input_layer=>buffer2, output_layer=>buffer3);
    lay3: out_pred GENERIC MAP (n_nodes_layer(2)) PORT MAP(input_layer=>buffer3, output_pred=>buffer4);
    back: out_back PORT MAP(input_pred=>buffer4, output_back=>buffer5);
    output <= dot_prod(buffer5,D);
END ARCHITECTURE;
	
CONFIGURATION nn_config OF nn IS
    FOR backward_nn
    END FOR;
END CONFIGURATION;