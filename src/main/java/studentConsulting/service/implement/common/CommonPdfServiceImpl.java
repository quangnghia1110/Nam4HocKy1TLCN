package studentConsulting.service.implement.common;


import com.lowagie.text.DocumentException;
import com.lowagie.text.pdf.BaseFont;
import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.text.PDFTextStripper;
import org.springframework.stereotype.Service;
import org.springframework.util.StreamUtils;
import org.springframework.web.multipart.MultipartFile;
import org.xhtmlrenderer.pdf.ITextRenderer;
import studentConsulting.service.interfaces.common.ICommonPdfService;

import javax.servlet.http.HttpServletResponse;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

@Service
public class CommonPdfServiceImpl implements ICommonPdfService {

    @Override
    public void generatePdfFromTemplate(String templatePath, Map<String, String> placeholders, String outputFileName, HttpServletResponse response) throws DocumentException, IOException {
        response.setContentType("application/pdf");
        response.setHeader("Content-Disposition", "attachment; filename=" + outputFileName + ".pdf");

        String htmlTemplate = loadHtmlTemplate(templatePath);

        String htmlContent = replacePlaceholdersInTemplate(htmlTemplate, placeholders);

        try (OutputStream outputStream = response.getOutputStream()) {
            convertHtmlToPdf(htmlContent, outputStream);
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

    private void convertHtmlToPdf(String htmlContent, OutputStream outputStream) throws IOException {
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

    @Override
    public String currentDate() {
        String pattern = "dd_MM_yyyy";
        SimpleDateFormat simpleDateFormat = new SimpleDateFormat(pattern);
        return simpleDateFormat.format(new Date());
    }

    @Override
    public <T> List<T> importFromPdf(MultipartFile file, Function<String, List<T>> parseFunction) throws IOException {
        String pdfContent = extractTextFromPdf(file);

        return parseFunction.apply(pdfContent);
    }

    private String extractTextFromPdf(MultipartFile file) throws IOException {
        try (PDDocument document = PDDocument.load(file.getInputStream())) {
            PDFTextStripper pdfStripper = new PDFTextStripper();
            return pdfStripper.getText(document);
        }
    }

}
