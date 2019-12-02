def fst():
    print(sum(map(lambda line: int(line.strip()) // 3 - 2, open('day1.input').readlines())))

def snd():
    def getfuel(n):
        fuel = n // 3 - 2
        return 0 if fuel <= 0 else fuel + getfuel(fuel)
    print(sum(map(lambda line: getfuel(int(line.strip())), open('day1.input').readlines())))