package aoc;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;

import static java.nio.charset.StandardCharsets.UTF_8;;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;

import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.LogManager;

public class Input {
    private static Logger logger = LogManager.getLogger(Input.class);

    static String simpleRead(int problem, boolean example) throws IOException {
        Path path = buildPath(problem, example);
        return Files.readString(path, UTF_8);
    }

    static Path buildPath(int problem, boolean example) {
        Path path = Path.of("..", "..");
        if (example) {
            path = path.resolve("examples");
        } else {
            path = path.resolve("inputs");
        }
        path = path.resolve(Integer.toString(problem)).toAbsolutePath().normalize();
        logger.debug("Path={}", path);
        return path;
    }
}
