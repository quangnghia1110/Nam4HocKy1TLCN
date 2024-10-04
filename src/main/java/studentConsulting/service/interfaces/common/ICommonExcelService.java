package studentConsulting.service.interfaces.common;

import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.List;

public interface ICommonExcelService {
    String currentDate();

    void generateExcelFile(String sheetName, List<String> headers, List<List<String>> data, String outputFileName, HttpServletResponse response) throws IOException;

}
