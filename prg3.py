def func3():
    list1=["Alaska", "Alabama", "Arkansas", "Arizona", "California", "Colorado", "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Iowa", "Idaho", "Illinois", "Kansas", "Kentucky", "Louisiana", "Massachusetts", "Maryland", "Maine", "Michigan", "Minnesota", "Missouri", "Mississippi", "Montana", "North Carolina", "North Dakota", "Nebraska", "New Hampshire", "New Jersey", "New Mexico", "Nevada", "New York", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennesse", "Texas", "Utah", "Virginia", "Vermont", "Washington", "Wisconsin", "West Virginia", "Wyoming"]
    list2=["yes_2013","no_2013","yes_2014","no_2014","yes_2015","no_2015"]
    list3=[]
    ReadCount=open("C:\Users\shalabh_singh\Desktop\\vasanth\\trial.csv", "r")
    WriteCount = open("C:\Users\shalabh_singh\Desktop\\vasanth\\result.csv", "w")
    with ReadCount as entry:
        for line in entry:
            temp=[]
            arr = line.strip("\n").split(",")
            for x in range(0, len(arr)):
                temp.append(arr[x])
            list3.append(temp)
    print list3
    print len(list1)

    for y in range(0, len(list1)):
        temp=list3[y]
        for x in range(0, len(list2)):
            line=list1[y]+","+list2[x]+","+temp[x]
            WriteCount.write(line)
            WriteCount.write("\n")




func3()