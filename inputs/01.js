/**
 * How do I read a file in Prolog? Let's just generate a PL document...
 */
import { readFileSync, writeFileSync } from "fs";
const data = readFileSync(`${import.meta.dirname}/01a.txt`, {encoding: "utf8"})
const lines = data.split("\n").map(line => line.split(/\s+/, 2).map(i => parseInt(i, 10)));
const list1 = []
const list2 = []
for (const line of lines) {
    if (line.length !== 2) {
        continue
    }
    const a = line[0]
    const b = line[1]
    list1.push(a)
    list2.push(b)
}
writeFileSync(`${import.meta.dirname}/01.pl`, `input(day1, [[${list1}],[${list2}]]).\n`)
