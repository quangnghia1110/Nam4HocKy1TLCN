package studentConsulting.service.interfaces.common;

import com.lowagie.text.DocumentException;

import java.io.IOException;
import java.io.OutputStream;
import java.util.Map;

public interface IPdfService {
    void generatePdfFromTemplate(String templatePath, Map<String, String> placeholders, OutputStream outputStream) throws DocumentException, IOException;

    String currentDate();
}
