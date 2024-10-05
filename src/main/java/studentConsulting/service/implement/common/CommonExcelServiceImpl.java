package studentConsulting.service.implement.common;

import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;
import studentConsulting.service.interfaces.common.ICommonExcelService;

import javax.servlet.http.HttpServletResponse;
import java.io.*;
import java.nio.charset.StandardCharsets;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

@Service
public class CommonExcelServiceImpl implements ICommonExcelService {


    @Override
    public void generateExcelFile(String sheetName, List<String> headers, List<List<String>> data, String outputFileName, HttpServletResponse response) throws IOException {

        String filePath = "J:\\DoAnGitHub\\Nam4HocKy1TLCN\\src\\main\\resources\\excel\\" + outputFileName + ".csv";

        try (OutputStreamWriter fileWriter = new OutputStreamWriter(new FileOutputStream(filePath), StandardCharsets.UTF_8)) {
            fileWriter.write('\uFEFF');

            fileWriter.write(String.join(",", headers));
            fileWriter.write("\n");

            for (List<String> rowData : data) {
                fileWriter.write(String.join(",", rowData));
                fileWriter.write("\n");
            }

            fileWriter.flush();
        } catch (IOException e) {
            throw new IOException("Lỗi khi lưu file CSV vào hệ thống cục bộ", e);
        }

        response.setContentType("text/csv; charset=UTF-8");
        response.setHeader("Content-Disposition", "attachment; filename=" + outputFileName + ".csv");

        try (OutputStreamWriter responseWriter = new OutputStreamWriter(response.getOutputStream(), StandardCharsets.UTF_8)) {
            responseWriter.write('\uFEFF');

            responseWriter.write(String.join(",", headers));
            responseWriter.write("\n");

            for (List<String> rowData : data) {
                responseWriter.write(String.join(",", rowData));
                responseWriter.write("\n");
            }

            responseWriter.flush();
        } catch (IOException e) {
            throw new IOException("Lỗi khi gửi file CSV qua HTTP response", e);
        }
    }


    @Override
    public String currentDate() {
        String pattern = "dd_MM_yyyy";
        SimpleDateFormat simpleDateFormat = new SimpleDateFormat(pattern);
        return simpleDateFormat.format(new Date());
    }

    @Override
    public List<List<String>> importCsv(MultipartFile file) throws IOException {
        List<String> fileContent = readFileContent(file);

        return parseCsvData(fileContent);
    }

    public List<String> readFileContent(MultipartFile file) throws IOException {
        try (BufferedReader reader = new BufferedReader(new InputStreamReader(file.getInputStream(), StandardCharsets.UTF_8))) {
            return reader.lines().collect(Collectors.toList());
        }
    }

    public List<List<String>> parseCsvData(List<String> fileContent) {
        return fileContent.stream()
                .map(line -> List.of(line.split(",")))
                .collect(Collectors.toList());
    }
}

