package studentConsulting.controller.common;

import com.lowagie.text.DocumentException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;
import studentConsulting.model.payload.dto.department_field.DepartmentDTO;
import studentConsulting.model.payload.dto.department_field.FieldDTO;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.service.interfaces.common.ICommonDepartmentService;
import studentConsulting.service.interfaces.common.ICommonExcelService;
import studentConsulting.service.interfaces.common.ICommonPdfService;

import javax.servlet.http.HttpServletResponse;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

@RestController
@RequestMapping("${base.url}")
public class CommonDepartmentController {

    @Autowired
    private ICommonDepartmentService departmentService;

    @Autowired
    private ICommonExcelService excelService;

    @Autowired
    private ICommonPdfService pdfService;

    @GetMapping("/list-department")
    public ResponseEntity<DataResponse<List<DepartmentDTO>>> getAllDepartments() {
        List<DepartmentDTO> departments = departmentService.getAllDepartments();
        DataResponse<List<DepartmentDTO>> response = DataResponse.<List<DepartmentDTO>>builder()
                .status("success")
                .message("Fetched all departments successfully.")
                .data(departments)
                .build();

        return ResponseEntity.ok(response);
    }

    @GetMapping("/list-field-by-department")
    public ResponseEntity<DataResponse<List<FieldDTO>>> getFieldsByDepartment(@RequestParam Integer departmentId) {
        List<FieldDTO> fields = departmentService.getFieldsByDepartment(departmentId);
        DataResponse<List<FieldDTO>> response = DataResponse.<List<FieldDTO>>builder()
                .status("success")
                .message("Fetched fields for department ID: " + departmentId + " successfully.")
                .data(fields)
                .build();

        return ResponseEntity.ok(response);
    }

    @PostMapping("/export-departments-csv")
    public void exportDepartmentsToExcel(HttpServletResponse response) throws IOException {
        List<DepartmentDTO> departments = departmentService.getAllDepartments();
        List<String> headers = List.of("Department ID", "Department Name");
        List<List<String>> data = departments.stream()
                .map(department -> List.of(department.getId().toString(), department.getName()))
                .collect(Collectors.toList());

        String fileName = "Departments_" + excelService.currentDate();
        excelService.generateExcelFile("Departments", headers, data, fileName, response);
    }

    @PostMapping("/export-fields-csv")
    public void exportFieldsByDepartmentToExcel(@RequestParam Integer departmentId, HttpServletResponse response) throws IOException {
        List<FieldDTO> fields = departmentService.getFieldsByDepartment(departmentId);
        List<String> headers = List.of("Field ID", "Field Name");
        List<List<String>> data = fields.stream()
                .map(field -> List.of(field.getId().toString(), field.getName()))
                .collect(Collectors.toList());

        String fileName = "Fields_" + excelService.currentDate();
        excelService.generateExcelFile("Fields", headers, data, fileName, response);
    }

    @PostMapping("/export-departments-pdf")
    public void exportDepartmentsToPdf(HttpServletResponse response) throws DocumentException, IOException {
        List<DepartmentDTO> departments = departmentService.getAllDepartments();
        String templatePath = "/templates/department_template.html";

        String dataRows = buildDepartmentDataRows(departments);

        Map<String, String> placeholders = Map.of(
                "{{date}}", pdfService.currentDate(),
                "{{departments}}", dataRows,
                "{{logo_url}}", "https://lh4.googleusercontent.com/proxy/L8S29oTCuu_R0eqZH-cnWHvW0nrEa-ZHILpFb2btfiQRbL5vzZ01TiT8WyaG2B8mMguiuV_WYnpHDzCjzZrUNTI83UNg6tL1K4I1uViJ9-tl_CJeZoIwmY5rYA"
        );

        String fileName = "Departments_" + pdfService.currentDate() + ".pdf";
        String outputFilePath = "J:\\DoAnGitHub\\Nam4HocKy1TLCN\\src\\main\\resources\\pdf\\" + fileName;

        try (OutputStream fileOutputStream = new FileOutputStream(outputFilePath)) {
            pdfService.generatePdfFromTemplate(templatePath, placeholders, fileOutputStream);
        } catch (IOException | DocumentException e) {
            throw new IOException("Lỗi khi lưu file PDF vào hệ thống cục bộ", e);
        }
        try (OutputStream responseStream = response.getOutputStream()) {
            pdfService.generatePdfFromTemplate(templatePath, placeholders, responseStream);  // Gửi PDF qua HTTP
            response.flushBuffer();
        } catch (IOException | DocumentException e) {
            throw new IOException("Lỗi khi gửi file PDF qua HTTP response", e);
        }
    }

    private String buildDepartmentDataRows(List<DepartmentDTO> departments) {
        StringBuilder dataRows = new StringBuilder();

        for (DepartmentDTO dept : departments) {
            dataRows.append("<tr>")
                    .append("<td>").append(dept.getId()).append("</td>")
                    .append("<td>").append(dept.getName()).append("</td>")
                    .append("</tr>");
        }

        return dataRows.toString();
    }

    @PostMapping("/export-fields-pdf")
    public void exportFieldsByDepartmentToPdf(@RequestParam Integer departmentId, HttpServletResponse response) throws DocumentException, IOException {
        List<FieldDTO> fields = departmentService.getFieldsByDepartment(departmentId);
        String templatePath = "/templates/fields_template.html";

        String dataRows = buildFieldDataRows(fields);

        Map<String, String> placeholders = Map.of(
                "{{date}}", pdfService.currentDate(),
                "{{fields}}", dataRows,
                "{{logo_url}}", "https://lh4.googleusercontent.com/proxy/L8S29oTCuu_R0eqZH-cnWHvW0nrEa-ZHILpFb2btfiQRbL5vzZ01TiT8WyaG2B8mMguiuV_WYnpHDzCjzZrUNTI83UNg6tL1K4I1uViJ9-tl_CJeZoIwmY5rYA"

        );
        String fileName = "Fields_" + pdfService.currentDate() + ".pdf";
        String outputFilePath = "J:\\DoAnGitHub\\Nam4HocKy1TLCN\\src\\main\\resources\\pdf\\" + fileName;

        try (OutputStream fileOutputStream = new FileOutputStream(outputFilePath)) {
            pdfService.generatePdfFromTemplate(templatePath, placeholders, fileOutputStream);
        } catch (IOException | DocumentException e) {
            throw new IOException("Lỗi khi lưu file PDF vào hệ thống cục bộ", e);
        }
        try (OutputStream responseStream = response.getOutputStream()) {
            pdfService.generatePdfFromTemplate(templatePath, placeholders, responseStream);  // Gửi PDF qua HTTP
            response.flushBuffer();
        } catch (IOException | DocumentException e) {
            throw new IOException("Lỗi khi gửi file PDF qua HTTP response", e);
        }
    }

    private String buildFieldDataRows(List<FieldDTO> fields) {
        StringBuilder dataRows = new StringBuilder();

        for (FieldDTO field : fields) {
            dataRows.append("<tr>")
                    .append("<td>").append(field.getId()).append("</td>")
                    .append("<td>").append(field.getName()).append("</td>")
                    .append("</tr>");
        }

        return dataRows.toString();
    }

    @PostMapping("/import-departments-csv")
    public ResponseEntity<?> importDepartmentsFromCsv(@RequestParam("file") MultipartFile file) throws IOException {
        List<List<String>> csvData = excelService.importCsv(file);
        departmentService.importDepartments(csvData);

        return ResponseEntity.ok(DataResponse.builder()
                .status("success")
                .message("Import thành công.")
                .build());
    }

    @PostMapping("/import-fields-csv")
    public ResponseEntity<?> importFieldsFromCsv(@RequestParam("file") MultipartFile file) throws IOException {
        List<List<String>> csvData = excelService.importCsv(file);
        departmentService.importFields(csvData);

        return ResponseEntity.ok(DataResponse.builder()
                .status("success")
                .message("Import thành công.")
                .build());
    }
}
