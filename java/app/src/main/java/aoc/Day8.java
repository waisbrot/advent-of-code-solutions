package aoc;

import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.LogManager;

public class Day8 implements Solution {
    private static Logger logger = LogManager.getLogger(Input.class);
    private Forest forest;

    enum Visibility {
        UNKNOWN, VISIBLE, INVISIBLE
    }

    static class Tree {
        // Tree north, south, east, west;
        int height;
        Visibility visible=Visibility.UNKNOWN;
        @Override
        public String toString() {
            return "Tree{"+height+":"+visible+"}";
        }
    }

    static class Forest {
        private final ArrayList<ArrayList<Tree>> grid;

        public Forest(ArrayList<ArrayList<Tree>> rows) {
            this.grid = rows;
        }

        static Forest read(int problem, boolean example) throws IOException {
            ArrayList<ArrayList<Tree>> rows = new ArrayList<>();
            try (FileReader reader = new FileReader(Input.buildPath(problem, example).toFile())) {
                ArrayList<Tree> currentRow = new ArrayList<>();
                int ci;
                while ((ci = reader.read()) != -1) {
                    char c = (char)ci;
                    if (c == '\n') {
                        rows.add(currentRow);
                        currentRow = new ArrayList<>();
                    } else {
                        Tree t = new Tree();
                        t.height = c - 0x30;
                        currentRow.add(t);
                    }
                }
                rows.add(currentRow);
                return new Forest(rows);
            }
        }

        void lookNSEW() {
            int rmax = grid.size();
            int cmax = grid.get(0).size();
            logger.debug("initial forest=\n{}", this);
            lookRightLeftHelper(rmax, cmax, 0, 1); // look right
            logger.debug("after look right forest=\n{}", this);
            lookRightLeftHelper(-1, -1, cmax-1, -1); // look left
            logger.debug("after look left forest=\n{}", this);
            lookUpDownHelper(rmax, cmax, 0, 1); // look down
            lookUpDownHelper(-1, -1, rmax-1, -1); // look up
        }

        private void lookRightLeftHelper(int rmax, int cmax, int start, int step) {
            for (int r = start; r != rmax; r += step) {
                int tallestSeen = -1;
                for (int c = start; c != cmax; c += step) {
                    Tree tree = grid.get(r).get(c);
                    if (tree.height > tallestSeen) {
                        tree.visible = Visibility.VISIBLE;
                        tallestSeen = tree.height;
                    }
                }
            }
        }

        private void lookUpDownHelper(int rmax, int cmax, int start, int step) {
            for (int c = start; c != rmax; c += step) {
                int tallestSeen = -1;
                for (int r = start; r != cmax; r += step) {
                    Tree tree = grid.get(r).get(c);
                    if (tree.height > tallestSeen) {
                        tree.visible = Visibility.VISIBLE;
                        tallestSeen = tree.height;
                    }
                }
            }
        }

        int countVisible() {
            int count = 0;
            for (ArrayList<Tree> row : grid) {
                for (Tree tree : row) {
                    if (tree.visible == Visibility.VISIBLE) {
                        count++;
                    }
                }
            }
            return count;
        }

        @Override
        public String toString() {
            StringBuilder sb = new StringBuilder();
            for (ArrayList<Tree> trees : grid) {
                for (Tree tree : trees) {
                    sb.append(tree).append(' ');
                }
                sb.append('\n');
            }
            return sb.toString();
        }

        public int maxScenicScore() {
            int maxScore = 0;
            int rmax = grid.size();
            int cmax = grid.get(0).size();
            for (int row = 0; row < rmax; row++) {
                for (int col = 0; col < cmax; col++) {
                    int east = scoreNSEW(row, col, 0, 1, cmax); //to the east
                    int west = scoreNSEW(row, col, 0, -1, -1); // to the west
                    int south = scoreNSEW(row, col, 1, 0, rmax); // south
                    int north = scoreNSEW(row, col, -1, 0, -1); // north
                    int score = north * south * east * west;
                    if (score > maxScore) {
                        maxScore = score;
                    }
                }
            }
            return maxScore;
        }

        private int scoreNSEW(int row, int col, int incRow, int incCol, int limit) {
            int treeCount = 0;
            int cutHeight = grid.get(row).get(col).height;
            while ((row += incRow) != limit && (col += incCol) != limit) {
                treeCount++;
                if (grid.get(row).get(col).height >= cutHeight) {
                    break;
                }
            }
            return treeCount;
        }
    }

    @Override
    public void loadInput(boolean example) throws IOException {
        forest = Forest.read(8, example);
        // logger.debug("grid={}", grid);
    }

    @Override
    public String solution1() {
        forest.lookNSEW();
        logger.debug("solution1 forest=\n{}", forest);
        return Integer.toString(forest.countVisible());
    }

    @Override
    public String solution2() {
        return Integer.toString(forest.maxScenicScore());
    }
}
