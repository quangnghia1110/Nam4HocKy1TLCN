package studentConsulting.service.interfaces.common;

import com.lowagie.text.DocumentException;
import org.springframework.web.multipart.MultipartFile;

import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

public interface ICommonPdfService {
    void generatePdfFromTemplate(String templatePath, Map<String, String> placeholders, String outputFileName, HttpServletResponse response) throws DocumentException, IOException;

    String currentDate();

    <T> List<T> importFromPdf(MultipartFile file, Function<String, List<T>> parseFunction) throws IOException;

}
