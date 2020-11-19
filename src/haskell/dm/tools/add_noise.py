import argparse
import random

def main(args):
    d = []
    n = ""
    name = []
    with open(args.input_file, 'r') as reader:
        i = 0
        for line in reader:
            if i == 0:
                n = line.split()[0]
                i += 1
                continue
            l = []

            j = 0
            for word in line.split():
                if j == 0:
                    name.append(word)
                    j += 1
                    continue
                l.append(float(word))
                j += 1
            d.append(l)
            i += 1
    print(len(d), len(d[0]))
    with open(args.output_file, 'w') as writer:
        writer.write(n + "\n")
        for i in range(len(d)):
            li = name[i]
            for j in range(len(d[0])):
                if i == j:
                    li += " 0.00 "
                elif i > j:
                    li += " " + str(d[j][i]) + " "
                else:
                    if args.rand == "uniform":
                        d[i][j] += random.uniform(-1,1)
                    elif args.rand == "gauss":
                        d[i][j] += random.gauss(0,1) 
                    li += " " + str(d[i][j]) + " "
            writer.write(li + "\n")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="")

    parser.add_argument("-i", "--input_file", type=str,
                        required=True,
                        help="Input distance file")
    

    parser.add_argument("-o", "--output_file", type=str,
                        required=True,
                        help="Output distance file")


    parser.add_argument("-r", "--rand", type=str,
                        required=True,
                        help="Output distance file")


    main(parser.parse_args())
