package studentConsulting.service.implement.common;

import com.lowagie.text.DocumentException;
import com.lowagie.text.pdf.BaseFont;
import org.springframework.stereotype.Service;
import org.springframework.util.StreamUtils;
import org.xhtmlrenderer.pdf.ITextRenderer;
import studentConsulting.service.interfaces.common.IPdfService;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Map;

@Service
public class PdfServiceImpl implements IPdfService {

    @Override
    public void generatePdfFromTemplate(String templatePath, Map<String, String> placeholders, OutputStream outputStream) throws DocumentException, IOException {
        String htmlTemplate = loadHtmlTemplate(templatePath);

        String htmlContent = replacePlaceholdersInTemplate(htmlTemplate, placeholders);

        try {
            ITextRenderer renderer = new ITextRenderer();

            renderer.getFontResolver().addFont("J:\\DoAnGitHub\\Nam4HocKy1TLCN\\src\\main\\resources\\font\\Arial.ttf", BaseFont.IDENTITY_H, BaseFont.EMBEDDED);

            renderer.setDocumentFromString(htmlContent);
            renderer.layout();
            renderer.createPDF(outputStream);
        } catch (Exception e) {
            throw new IOException("Lỗi khi chuyển đổi HTML sang PDF", e);
        }
    }

    private String loadHtmlTemplate(String filePath) throws IOException {
        try (InputStream inputStream = getClass().getResourceAsStream(filePath)) {
            if (inputStream == null) {
                throw new FileNotFoundException("File HTML không tồn tại: " + filePath);
            }
            return StreamUtils.copyToString(inputStream, StandardCharsets.UTF_8);
        }
    }

    private String replacePlaceholdersInTemplate(String htmlTemplate, Map<String, String> placeholders) {
        String modifiedTemplate = htmlTemplate;
        for (Map.Entry<String, String> entry : placeholders.entrySet()) {
            modifiedTemplate = modifiedTemplate.replace(entry.getKey(), entry.getValue());
        }
        return modifiedTemplate;
    }

    @Override
    public String currentDate() {
        String pattern = "dd_MM_yyyy_HH_mm_ss";
        SimpleDateFormat simpleDateFormat = new SimpleDateFormat(pattern);
        return simpleDateFormat.format(new Date());
    }
}
