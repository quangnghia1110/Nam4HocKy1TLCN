package studentConsulting.service.interfaces.common;

import com.lowagie.text.DocumentException;

import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.Map;

public interface ICommonPdfService {
    void generatePdfFromTemplate(String templatePath, Map<String, String> placeholders, String outputFileName, HttpServletResponse response) throws DocumentException, IOException;

    String currentDate();
}
