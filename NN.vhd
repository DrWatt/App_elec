LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.math_real.ALL;
USE work.float_pkg.ALL;
--------------------------- PACKAGE WITH TYPE DEFINITIONS AND EXPONENTIAL AND TANH DEFINITIONS
PACKAGE typpes IS
    TYPE FLOAT_VECTOR IS ARRAY (NATURAL RANGE <>) OF FLOAT32;
    TYPE matrix IS ARRAY (NATURAL RANGE <>) OF FLOAT_VECTOR;
    TYPE posint_array IS ARRAY (NATURAL RANGE <>) OF POSITIVE; 
    FUNCTION tanhh (SIGNAL s: FLOAT32) RETURN FLOAT32;
    FUNCTION sigmoid (SIGNAL s: FLOAT32) RETURN FLOAT32;
    --FUNCTION exp (s: FLOAT32) RETURN FLOAT32;
    FUNCTION exp2 (s: FLOAT32) RETURN FLOAT32;
    FUNCTION dot_prod (SIGNAL a: FLOAT_VECTOR; SIGNAL b: FLOAT_VECTOR) RETURN FLOAT32;
    FUNCTION subtrac (a: FLOAT_VECTOR;SIGNAL b: FLOAT_VECTOR) RETURN FLOAT_VECTOR;
    FUNCTION elementwise_square(a:FLOAT_VECTOR) RETURN FLOAT_VECTOR;
    FUNCTION colrowprod (a: FLOAT_VECTOR; b: FLOAT_VECTOR) RETURN matrix;
    FUNCTION transpose (a:matrix) RETURN matrix;
    FUNCTION testss (a:matrix) RETURN FLOAT32;
END PACKAGE;

PACKAGE BODY typpes IS

    --FUNCTION exp (s: FLOAT32) RETURN FLOAT32 IS
    --    VARIABLE e: FLOAT32 := "01000000001011011111100001010100";
    --    VARIABLE sp: FLOAT32:= s;
    --    VARIABLE int: INTEGER := TO_INTEGER(sp);
    --    VARIABLE re: FLOAT32 := sp-int;
    --    VARIABLE temp: FLOAT32:= TO_FLOAT(1);
    --    VARIABLE temp2: FLOAT32:= TO_FLOAT(1);
    --    VARIABLE COUNT: INTEGER:=0;
    --    BEGIN
    --        FOR i IN 0 TO int-1 LOOP
    --            temp:= temp*e;
    --        END LOOP;
    --        
    --        temp2 := TO_FLOAT(1)+re+(re*re)/TO_FLOAT(2)+(re*re*re)/TO_FLOAT(6)+(re*re*re*re)/TO_FLOAT(24)+(re*re*re*re*re)/TO_FLOAT(120);
    --        
    --    RETURN temp*temp2;
    --    
    --END FUNCTION;
--------------------------- EXPONENTIAL EXP(2*X)
    FUNCTION exp2 (s: FLOAT32) RETURN FLOAT32 IS
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
    FUNCTION tanhh (SIGNAL s: FLOAT32) RETURN FLOAT32 IS
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
    -----------------------------SIGMOID
    FUNCTION sigmoid (SIGNAL s: FLOAT32) RETURN FLOAT32 IS
    VARIABLE a,b:FLOAT32;
    VARIABLE meno:FLOAT32 := TO_FLOAT(-1);
    VARIABLE expmeno:FLOAT32 := exp2(meno);
    BEGIN
    a := RECIPROCAL(exp2(s)*expmeno);
    b := a + TO_FLOAT(1);
    RETURN RECIPROCAL(b);
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
    
    FUNCTION colrowprod (a: FLOAT_VECTOR; b: FLOAT_VECTOR) RETURN matrix IS
    VARIABLE temp: matrix(b'RANGE)(a'RANGE);
    BEGIN
    FOR i IN b'RANGE LOOP
        FOR j IN a'RANGE LOOP
            temp(i)(j) := b(i) * a(j);
        END LOOP;
    END LOOP;
    RETURN temp;
    END FUNCTION;

    FUNCTION transpose (a:matrix) RETURN matrix IS
    VARIABLE temp: matrix(a(0)'RANGE)(a'RANGE);
    BEGIN

    FOR i IN a'RANGE LOOP
        FOR j IN a(0)'RANGE LOOP
        temp(j)(i) := a(i)(j);
        END LOOP;
    END LOOP;
    RETURN temp;
    END FUNCTION;

    FUNCTION elementwise_square(a:FLOAT_VECTOR) RETURN FLOAT_VECTOR IS
    VARIABLE temp :FLOAT_VECTOR(a'RANGE);
    BEGIN
        FOR i IN a'RANGE LOOP
            temp(i) := a(i)*a(i);
        END LOOP;
    RETURN temp;
    END FUNCTION;



    FUNCTION testss (a:matrix) RETURN FLOAT32 IS
    VARIABLE temp: FLOAT32;
    BEGIN
    FOR i IN a(0)'RANGE LOOP
    FOR j IN a'RANGE LOOP
    temp := temp + a(j)(i);
    END LOOP;
    END LOOP;
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
     	weights: INOUT matrix(nodes-1 DOWNTO 0)(in_dim-1 DOWNTO 0);
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
     	weights: INOUT matrix(0 DOWNTO 0)(in_dim-1 DOWNTO 0);
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
        layin3n: POSITIVE;
        layin2n: POSITIVE;
        layin1n: POSITIVE;
        layin0n: POSITIVE;
        N: POSITIVE := 3;
        eta: FLOAT32 := TO_FLOAT(0.3));
    PORT(
        input_pred: IN FLOAT32;
        input_lay3: IN FLOAT_VECTOR(layin3n-1 DOWNTO 0);
        input_lay2: IN FLOAT_VECTOR(layin2n-1 DOWNTO 0);
        input_lay1: IN FLOAT_VECTOR(layin1n-1 DOWNTO 0);
        input_lay0: IN FLOAT_VECTOR(layin0n-1 DOWNTO 0);
        input_true: IN FLOAT32;
        weight_in: IN matrix(0 DOWNTO 0)(layin3n-1 DOWNTO 0);
        output_weight: OUT FLOAT_VECTOR(layin3n-1 DOWNTO 0);
        output_delta: OUT FLOAT_VECTOR(N-1 DOWNTO 0);
        lay3input_buffer: OUT matrix(layin3n-1 DOWNTO 0)(N-1 DOWNTO 0);
        lay2input_buffer: OUT matrix(layin2n-1 DOWNTO 0)(N-1 DOWNTO 0);
        lay1input_buffer: OUT matrix(layin1n-1 DOWNTO 0)(N-1 DOWNTO 0);
        lay0input_buffer: OUT matrix(layin0n-1 DOWNTO 0)(N-1 DOWNTO 0);
        clk: IN BIT:= '0'
        );
END ENTITY;
--------------------------------------------------
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.math_real.ALL;
USE work.typpes.ALL;
USE work.float_pkg.ALL;
USE work.fixed_pkg.ALL;

ENTITY layer_backpropagation IS
    GENERIC(
        in_dim,nodes,right_nodes: POSITIVE;
        eta: FLOAT32:=TO_FLOAT(0.3);
        N:POSITIVE :=3);
    PORT(
        weight_in: INOUT matrix(nodes-1 DOWNTO 0)(in_dim-1 DOWNTO 0);
        delta: IN matrix(right_nodes-1 DOWNTO 0)(N-1 DOWNTO 0);
        pred_val: IN matrix(nodes-1 DOWNTO 0)(N-1 DOWNTO 0);
        input_data: IN matrix(in_dim -1 DOWNTO 0)(N-1 DOWNTO 0);
        bias: IN FLOAT_VECTOR(nodes-1 DOWNTO 0);
        right_layer_weight: IN matrix(right_nodes-1 DOWNTO 0)(nodes-1 DOWNTO 0);
        weight_out: OUT matrix(nodes-1 DOWNTO 0)(in_dim-1 DOWNTO 0);
        out_delta: OUT matrix(nodes-1 DOWNTO 0)(N-1 DOWNTO 0));
END ENTITY;
--------------------------------
ARCHITECTURE lay_behavioural OF layer IS
    SIGNAL out_temp: FLOAT_VECTOR(nodes -1 DOWNTO 0);
    BEGIN
        gen: FOR i IN output_layer'RANGE GENERATE
            out_temp(i) <= dot_prod(input_layer,weights(i)) + bias;
            output_layer(i) <= tanhh(out_temp(i));
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
    VARIABLE bufferr3: matrix(layin3n-1 DOWNTO 0)(N-1 DOWNTO 0);
    VARIABLE bufferr2: matrix(layin2n-1 DOWNTO 0)(N-1 DOWNTO 0);
    VARIABLE bufferr1: matrix(layin1n-1 DOWNTO 0)(N-1 DOWNTO 0);
    VARIABLE bufferr0: matrix(layin0n-1 DOWNTO 0)(N-1 DOWNTO 0);
    VARIABLE bufferr3sum: FLOAT32;
    VARIABLE bufferr3transpose: matrix(N-1 DOWNTO 0)(layin3n-1 DOWNTO 0);
    VARIABLE buffweight: matrix(0 DOWNTO 0)(layin3n-1 DOWNTO 0);
    VARIABLE buffweight2: FLOAT_VECTOR(layin3n-1 DOWNTO 0);
    VARIABLE preprodtemp:FLOAT_VECTOR(N-1 DOWNTO 0);
    BEGIN
        FOR i IN N-1 DOWNTO 0 LOOP
            IF i = N-1 THEN
                bufferr(i) := input_true - input_pred;
            ELSIF (bufferr(i+1) /= input_pred) THEN
                bufferr(i) := input_true - input_pred;
                FOR j IN layin3n - 1 DOWNTO 0 LOOP
                    bufferr3(j)(i) := input_lay3(j);
                END LOOP;
                FOR j IN layin2n - 1 DOWNTO 0 LOOP
                    bufferr2(j)(i) := input_lay2(j);
                END LOOP;
                FOR j IN layin1n - 1 DOWNTO 0 LOOP
                    bufferr1(j)(i) := input_lay1(j);
                END LOOP;
                FOR j IN layin0n - 1 DOWNTO 0 LOOP
                    bufferr0(j)(i) := input_lay0(j);
                END LOOP;
            END IF;
        lay3input_buffer <= bufferr3;
        lay2input_buffer <= bufferr2;
        lay1input_buffer <= bufferr1;
        lay0input_buffer <= bufferr0;
        --cap := cap + 1;
        --IF cap = 10 THEN count:= N; END IF;
        END LOOP;


        FOR i IN N-1 DOWNTO 0 LOOP
            FOR j IN layin3n-1 DOWNTO 0 LOOP
                bufferr3sum := bufferr3sum + bufferr3(j)(i);
            END LOOP;
            preprodtemp(i):= bufferr(i)*bufferr3sum;
            bufferr3sum := TO_FLOAT(0);
        END LOOP;
        output_delta <= preprodtemp;
        FOR i IN N-1 DOWNTO 0 LOOP
            
            FOR j IN buffweight(0)'RANGE LOOP
                buffweight2(j) := bufferr3transpose(i)(j)* preprodtemp(i);
                buffweight(0)(j) := buffweight(0)(j) + buffweight2(j);
            END LOOP;
        END LOOP;

        FOR i IN buffweight(0)'RANGE LOOP
            buffweight(0)(i) := buffweight(0)(i)*eta;
            buffweight(0)(i) := buffweight(0)(i)/TO_FLOAT(N);
            buffweight(0)(i) := weight_in(0)(i)-buffweight(0)(i);
        END LOOP;
        ----FOR i IN bufferr'RANGE LOOP
        --    bufferr
        --END LOOP;
        output_weight <= buffweight(0);
    END PROCESS;

END ARCHITECTURE;

ARCHITECTURE backprop OF layer_backpropagation IS
BEGIN
PROCESS(delta)
VARIABLE prev_del_weighted: matrix(nodes-1 DOWNTO 0)(N-1 DOWNTO 0);
VARIABLE sigma: matrix(nodes-1 DOWNTO 0)(N-1 DOWNTO 0);
VARIABLE temp_delta: matrix(nodes-1 DOWNTO 0)(N-1 DOWNTO 0);
VARIABLE weight_correction: matrix(nodes-1 DOWNTO 0)(in_dim-1 DOWNTO 0);
VARIABLE temp_weight: matrix(nodes-1 DOWNTO 0)(in_dim-1 DOWNTO 0);  
VARIABLE pred_val_squared: matrix(nodes-1 DOWNTO 0)(N-1 DOWNTO 0);
VARIABLE norm_eta: FLOAT32 := -eta/N;
VARIABLE transposed_input:matrix(N -1 DOWNTO 0)(in_dim-1 DOWNTO 0) := transpose(input_data);
BEGIN
FOR i IN N-1 DOWNTO 0 LOOP
    FOR k IN right_layer_weight'RANGE LOOP
        FOR j IN right_layer_weight(k)'RANGE LOOP
        
            prev_del_weighted(j)(i) := prev_del_weighted(j)(i) + right_layer_weight(k)(j)*delta(k)(i);
        END LOOP;
    END LOOP;
END LOOP;

FOR i IN pred_val'RANGE LOOP
pred_val_squared(i) := elementwise_square(pred_val(i));
END LOOP;

FOR i IN pred_val(0)'RANGE LOOP
    FOR j IN pred_val_squared'RANGE LOOP
        FOR k IN input_data'RANGE LOOP
            sigma(j)(i) := sigma(j)(i)+pred_val_squared(j)(i)*input_data(k)(i);
        END LOOP;
    END LOOP;
END LOOP;

FOR i in sigma'RANGE LOOP
    FOR j IN sigma(0)'RANGE LOOP
    sigma(i)(j) := 1 - sigma(i)(j);
    END LOOP;
END LOOP;

FOR i in sigma'RANGE LOOP
    FOR j IN sigma(0)'RANGE LOOP
    temp_delta(i)(j) := prev_del_weighted(i)(j) * sigma(i)(j);
    END LOOP;
END LOOP;
out_delta <= temp_delta;

FOR i IN weight_correction'RANGE LOOP
    FOR j IN weight_correction(0)'RANGE LOOP
        FOR k IN N-1 DOWNTO 0 LOOP
        weight_correction(i)(j) := weight_correction(i)(j) + transposed_input(k)(j)*temp_delta(i)(k);
        END LOOP;
    END LOOP;
END LOOP;

FOR i IN weight_correction'RANGE LOOP
    FOR j IN weight_correction(0)'RANGE LOOP
    weight_correction(i)(j) := weight_correction(i)(j)*norm_eta;
    END LOOP;
END LOOP;

FOR i IN temp_weight'RANGE LOOP
    FOR j IN temp_weight(0)'RANGE LOOP
    temp_weight(i)(j) := weight_in(i)(j) - weight_correction(i)(j);
    END LOOP;
END LOOP;
weight_out <= temp_weight;
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
    --SIGNAL buffer4: FLOAT32;

    BEGIN
    --layin: in_layer GENERIC MAP(M,N) PORT MAP(input_da=>input_data,output_layer=>buffer0);
    lay0: layer GENERIC MAP (M,n_nodes_layer(0)) PORT MAP(input_layer=>input_data, output_layer=>buffer1);
    lay1: layer GENERIC MAP (n_nodes_layer(0),n_nodes_layer(1)) PORT MAP(input_layer=>buffer1, output_layer=>buffer2);
    lay2: layer GENERIC MAP (n_nodes_layer(1),n_nodes_layer(2)) PORT MAP(input_layer=>buffer2, output_layer=>buffer3);
    lay3: out_pred GENERIC MAP (n_nodes_layer(2)) PORT MAP(input_layer=>buffer3, output_pred=>output);
    --output<=(OTHERS=>buffer4);
END ARCHITECTURE;
-------------------------------------------------------------

ARCHITECTURE backward_nn OF nn IS
    COMPONENT layer IS
        GENERIC (in_dim,nodes: POSITIVE);
        PORT(
            input_layer: IN FLOAT_VECTOR(in_dim -1 DOWNTO 0);
         	weights: INOUT matrix(nodes-1 DOWNTO 0)(in_dim-1 DOWNTO 0) := (OTHERS=>(OTHERS=>TO_FLOAT(1.0,8,23)));
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
         	weights: INOUT matrix(0 DOWNTO 0)(in_dim-1 DOWNTO 0) := (OTHERS=>(OTHERS=>TO_FLOAT(1.0,8,23)));
         	bias: IN FLOAT32 := "00000000000000000000000000000000";
            output_pred: OUT FLOAT32
    	   );
    END COMPONENT;

    COMPONENT out_back IS
        GENERIC (
        layin3n: POSITIVE;
        layin2n: POSITIVE;
        layin1n: POSITIVE;
        layin0n: POSITIVE;
        N: POSITIVE := 3;
        eta: FLOAT32 := TO_FLOAT(0.3));
    PORT(
        input_pred: IN FLOAT32;
        input_lay3: IN FLOAT_VECTOR(layin3n-1 DOWNTO 0);
        input_lay2: IN FLOAT_VECTOR(layin2n-1 DOWNTO 0);
        input_lay1: IN FLOAT_VECTOR(layin1n-1 DOWNTO 0);
        input_lay0: IN FLOAT_VECTOR(layin0n-1 DOWNTO 0);
        input_true: IN FLOAT32:= TO_FLOAT(1);
        weight_in: IN matrix(0 DOWNTO 0)(layin3n-1 DOWNTO 0):=(OTHERS=>(OTHERS=>TO_FLOAT(1.0,8,23)));
        output_weight: OUT FLOAT_VECTOR(layin3n-1 DOWNTO 0);
        output_delta: OUT FLOAT_VECTOR(N-1 DOWNTO 0);
        lay3input_buffer: OUT matrix(layin3n-1 DOWNTO 0)(N-1 DOWNTO 0);
        lay2input_buffer: OUT matrix(layin2n-1 DOWNTO 0)(N-1 DOWNTO 0);
        lay1input_buffer: OUT matrix(layin1n-1 DOWNTO 0)(N-1 DOWNTO 0);
        lay0input_buffer: OUT matrix(layin0n-1 DOWNTO 0)(N-1 DOWNTO 0);
        clk: IN BIT:= '0'
        );
    END COMPONENT;

    COMPONENT layer_backpropagation IS
    GENERIC(
        in_dim,nodes,right_nodes: POSITIVE;
        eta: FLOAT32:=TO_FLOAT(0.3);
        N:POSITIVE :=3);
    PORT(
        weight_in: INOUT matrix(nodes-1 DOWNTO 0)(in_dim-1 DOWNTO 0);
        delta: IN matrix(right_nodes-1 DOWNTO 0)(N-1 DOWNTO 0);
        pred_val: IN matrix(nodes-1 DOWNTO 0)(N-1 DOWNTO 0);
        input_data: IN matrix(in_dim -1 DOWNTO 0)(N-1 DOWNTO 0);
        bias: IN FLOAT_VECTOR(nodes-1 DOWNTO 0):=(OTHERS=>TO_FLOAT(0));
        right_layer_weight: IN matrix(right_nodes-1 DOWNTO 0)(nodes-1 DOWNTO 0);
        weight_out: OUT matrix(nodes-1 DOWNTO 0)(in_dim-1 DOWNTO 0);
        out_delta: OUT matrix(nodes-1 DOWNTO 0)(N-1 DOWNTO 0));
    END COMPONENT;
--SIGNAL buffer0: FLOAT_VECTOR(M-1 DOWNTO 0);
    SIGNAL buffer1: FLOAT_VECTOR(n_nodes_layer(0)-1 DOWNTO 0);
    SIGNAL buffer2: FLOAT_VECTOR(n_nodes_layer(1)-1 DOWNTO 0);
    SIGNAL buffer3: FLOAT_VECTOR(n_nodes_layer(2)-1 DOWNTO 0);
    SIGNAL buffer4: FLOAT32;
    SIGNAL buffer5: FLOAT_VECTOR(buffer3'LENGTH-1 DOWNTO 0);
    SIGNAL D: FLOAT_VECTOR(buffer3'LENGTH-1 DOWNTO 0) := (OTHERS=>TO_FLOAT(1.0,8,23));
    SIGNAL weight0: matrix(n_nodes_layer(0)-1 DOWNTO 0)(M-1 DOWNTO 0);
    SIGNAL weight1: matrix(n_nodes_layer(1)-1 DOWNTO 0)(n_nodes_layer(0)-1 DOWNTO 0);
    SIGNAL weight2: matrix(n_nodes_layer(2)-1 DOWNTO 0)(n_nodes_layer(1)-1 DOWNTO 0);
    SIGNAL weight3: matrix(0 DOWNTO 0)(n_nodes_layer(2)-1 DOWNTO 0);
    SIGNAL delta0: matrix(n_nodes_layer(0)-1 DOWNTO 0)(N-1 DOWNTO 0);
    SIGNAL delta1: matrix(n_nodes_layer(1)-1 DOWNTO 0)(N-1 DOWNTO 0);
    SIGNAL delta2: matrix(n_nodes_layer(2)-1 DOWNTO 0)(N-1 DOWNTO 0);
    SIGNAL delta3: matrix(0 DOWNTO 0)(N-1 DOWNTO 0);
    SIGNAL outweight0: matrix(n_nodes_layer(0)-1 DOWNTO 0)(M-1 DOWNTO 0);
    SIGNAL outweight1: matrix(n_nodes_layer(1)-1 DOWNTO 0)(n_nodes_layer(0)-1 DOWNTO 0);
    SIGNAL outweight2: matrix(n_nodes_layer(2)-1 DOWNTO 0)(n_nodes_layer(1)-1 DOWNTO 0);
    SIGNAL outweight3: FLOAT_VECTOR(n_nodes_layer(2)-1 DOWNTO 0);
    SIGNAL N3_buffer: matrix(n_nodes_layer(2)-1 DOWNTO 0)(N-1 DOWNTO 0);
    SIGNAL N2_buffer: matrix(n_nodes_layer(1)-1 DOWNTO 0)(N-1 DOWNTO 0);
    SIGNAL N1_buffer: matrix(n_nodes_layer(0)-1 DOWNTO 0)(N-1 DOWNTO 0);
    SIGNAL N0_buffer: matrix(M-1 DOWNTO 0)(N-1 DOWNTO 0);

BEGIN
    lay0: layer GENERIC MAP (M,n_nodes_layer(0)) PORT MAP(input_layer=>input_data, output_layer=>buffer1, weights=>weight0);
    lay1: layer GENERIC MAP (n_nodes_layer(0),n_nodes_layer(1)) PORT MAP(input_layer=>buffer1, output_layer=>buffer2, weights=>weight1);
    lay2: layer GENERIC MAP (n_nodes_layer(1),n_nodes_layer(2)) PORT MAP(input_layer=>buffer2, output_layer=>buffer3, weights=>weight2);
    lay3: out_pred GENERIC MAP (n_nodes_layer(2)) PORT MAP(input_layer=>buffer3, output_pred=>buffer4, weights=>weight3);
    back: out_back GENERIC MAP(layin0n => M,layin1n => buffer1'LENGTH,layin2n => buffer2'LENGTH,layin3n => buffer3'LENGTH) PORT MAP(input_pred=>buffer4,input_lay0=>input_data,input_lay1=>buffer1,input_lay2=>buffer2,input_lay3=>buffer3, weight_in=>weight3, output_weight=>outweight3,output_delta=>delta3(0),lay0input_buffer=>N0_buffer,lay1input_buffer=>N1_buffer,lay2input_buffer=>N2_buffer,lay3input_buffer=>N3_buffer);
    blay2: layer_backpropagation GENERIC MAP(n_nodes_layer(1),n_nodes_layer(2),1) PORT MAP(weight_in=>weight2,delta=>delta3,pred_val=>N3_buffer, input_data=> N2_buffer,right_layer_weight=>weight3,out_delta=>delta2,weight_out=>outweight2);
    blay1: layer_backpropagation GENERIC MAP(n_nodes_layer(0),n_nodes_layer(1),n_nodes_layer(2)) PORT MAP(weight_in=>weight1,delta=>delta2,pred_val=>N2_buffer, input_data=> N1_buffer,right_layer_weight=>weight2,out_delta=>delta1,weight_out=>outweight1);
    blay0: layer_backpropagation GENERIC MAP(M,n_nodes_layer(0),n_nodes_layer(1)) PORT MAP(weight_in=>weight0,delta=>delta1,pred_val=>N1_buffer, input_data=> N0_buffer,right_layer_weight=>weight1,out_delta=>delta0,weight_out=>outweight0);
    output <= testss(outweight0);
END ARCHITECTURE;
	
CONFIGURATION nn_config OF nn IS
    FOR backward_nn
    END FOR;
END CONFIGURATION;