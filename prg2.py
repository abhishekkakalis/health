


def prg2():
    ReadCount=open("C:\Users\shalabh_singh\Desktop\Socio Economic\\beds.csv","r")
    WriteCount = open("C:\Users\shalabh_singh\Desktop\Socio Economic\\beds1.csv", "w")
    dict1={}
    with ReadCount as entry:
        for line in entry:
            arr=line.strip("\n").split(",")
            if arr[0] not in dict1:
                dict1.update({arr[0]:[0,0,0,0,0,0]})
                list=[0,0,0,0,0,0]
                if arr[1] == "A":
                    list[0]+=1
                elif arr[1] == "B":
                    list[1] += 1
                elif arr[1] == "C":
                    list[2] += 1
                elif arr[1] == "D":
                    list[3] += 1
                elif arr[1] == "E":
                    list[4] += 1
                elif arr[1] == "F":
                    list[5] += 1
            else:
                if arr[1] == "A":
                    list[0] += 1
                elif arr[1] == "B":
                    list[1] += 1
                elif arr[1] == "C":
                    list[2] += 1
                elif arr[1] == "D":
                    list[3] += 1
                elif arr[1] == "E":
                    list[4] += 1
                elif arr[1] == "F":
                    list[5] += 1
            dict1.update({arr[0]:list})
    for key in dict1:
        #print key,dict1[key][0],dict1[key][1],dict1[key][2],dict1[key][3],dict1[key][4],dict1[key][5]
        line=key+","+str(dict1[key][0])+","+str(dict1[key][1])+","+str(dict1[key][2])+","+str(dict1[key][3])+","+str(dict1[key][4])+","+str(dict1[key][5])
        WriteCount.write(line)
        WriteCount.write("\n")


ExtractFoods()