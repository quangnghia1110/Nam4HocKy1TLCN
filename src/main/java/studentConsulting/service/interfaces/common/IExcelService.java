package studentConsulting.service.interfaces.common;

import org.springframework.web.multipart.MultipartFile;

import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.List;

public interface IExcelService {
    String currentDate();

    void generateExcelFile(String sheetName, List<String> headers, List<List<String>> data, String outputFileName, HttpServletResponse response) throws IOException;

    List<List<String>> importCsv(MultipartFile file) throws IOException;

}
