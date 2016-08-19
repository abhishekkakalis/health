def meth1():
    ReadCount=open(relativePath+"\Resource\Image_efficiency_2013_2014_2015_all_score_1.csv","r")
    WriteCount=open(relativePath+"\Resource\Image_efficiency_2013_2014_2015_all_score_2.csv","w")
    with ReadCount as entry:
        for line in entry:
            string=line.split(",")
            count=0
            size=len(string)
            for x in range(0, size):
                if not string[x]:
                    count+=1
                if count==4:
                    print string[6]
                    break
            if count<4:
                WriteCount.write(line)