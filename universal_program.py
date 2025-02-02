

def p(x):
    if x == 0:
        return 0
    else:
        return x - 1


def lp2(x):
    if x%2==0:
        return lp2(int(x/2)) + 1
    else:
        return 0

def left(x):
    return lp2(x+1)

def right(x):

    return int(  (  int( (x+1) / (2**lp2(x+1)) ) - 1  )  / 2 )



def find_label(label, program):
    for i in range(len(program)):
        if left(program[i]) == label:
            return i
    return -1




def printing(number, inputing, locals, y):
    print(number, end=" ")
    for i in inputing:
        print(i, end=" ")
    for i in locals:
        print(i, end=" ")
    print(y)

#x = int(input())
#print(left(x), " " ,right(x))

line_number = 1
inputs = []
local_variables = []
Y = 0

program = input().split()
inputs = input().split()
for i in range(len(program)):
    program[i] = int(program[i])
for i in range(len(inputs)):
    inputs[i] = int(inputs[i])


biggest_local = 0
biggest_input = 0
for item in program:
    variable = right(right(item))
    if variable % 2 == 0 and variable > biggest_local:
        biggest_local = variable
    elif variable % 2 == 1 and variable > biggest_input:
        biggest_input = variable

biggest_local = int(biggest_local / 2)
biggest_input = int((biggest_input+1 )/2)
if biggest_input > len(inputs):
    for i in range(biggest_input-len(inputs)):
        inputs.append(0)


for i in range(biggest_local):
    local_variables.append(0)

#print(biggest_input, " ", biggest_local)

for j in range(10):
    #print("----------------")
    #print("line number : ", line_number)
    #print("program instruction: " , program[line_number - 1])
    instruction = left(right(program[line_number - 1]))

    #print("instruction : ", instruction)
    #print("--------------")


    if instruction == 0:
        printing(line_number, inputs, local_variables, Y)

        line_number += 1

    elif instruction == 1:
        #print("plus")
        printing(line_number, inputs, local_variables, Y)
        variable = right(right(program[line_number - 1]))
        #print("variable : ", variable)
        if variable == 0:
            Y += 1
        elif variable % 2 == 0:
            local_variables[int(variable/2) -1] += 1
        else:
            inputs[int((variable-1)/2)] += 1

        line_number += 1

    elif instruction == 2:
        #print("minus")
        printing(line_number, inputs, local_variables, Y)
        variable = right(right(program[line_number - 1]))
        #print("variable: ", variable)
        if variable == 0:
            Y -= 1
        elif variable % 2 == 0:
            local_variables[int(variable/2) -1 ] -= 1
        else:
            inputs[int((variable-1)/2)] -= 1

        line_number += 1

    else:
        #print("jumping")
        #print("instruction: ", instruction)
        printing(line_number, inputs, local_variables, Y)
        variable = right(right(program[line_number - 1]))


        condition = False

        if variable == 0:
            if(Y != 0):
                condition = True
        elif variable % 2 == 0:
                if local_variables[int(variable/2) -1] != 0:
                    condition = True
        else:
            if inputs[int((variable-1)/2)] != 0:
                condition = True

        #print("condition: ", condition)
        if(condition == False):
            line_number += 1
            continue


        label = instruction-2
        #print("the label code:" , label)

        line_number = find_label(label, program)
        #print("line number : ", line_number)
        if line_number == -1:
            break

        line_number += 1
