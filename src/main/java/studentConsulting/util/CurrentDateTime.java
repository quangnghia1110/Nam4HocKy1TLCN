package studentConsulting.util;

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

public class CurrentDateTime {
    public static String getCurrentDateTime() {
        LocalDate now = LocalDate.now();
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd/mm/yyyy");
        return now.format(formatter);
    }
}
