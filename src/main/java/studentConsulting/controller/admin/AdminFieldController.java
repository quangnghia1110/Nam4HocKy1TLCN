package studentConsulting.controller.admin;

import com.lowagie.text.DocumentException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;
import studentConsulting.constant.FilePaths;
import studentConsulting.constant.SecurityConstants;
import studentConsulting.model.payload.dto.department_field.ManageFieldDTO;
import studentConsulting.model.payload.request.department_field.FieldRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.service.interfaces.admin.IAdminFieldService;
import studentConsulting.service.interfaces.common.ICommonExcelService;
import studentConsulting.service.interfaces.common.ICommonPdfService;

import javax.servlet.http.HttpServletResponse;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

@RestController
@RequestMapping("${base.url}")
public class AdminFieldController {

    @Autowired
    private IAdminFieldService fieldService;

    @Autowired
    private ICommonExcelService excelService;

    @Autowired
    private ICommonPdfService pdfService;

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/admin/field/list")
    public ResponseEntity<DataResponse<Page<ManageFieldDTO>>> getFields(
            @RequestParam(required = false) Optional<String> name,
            @RequestParam(required = false) Optional<String> departmentId,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "id") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir) {

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

        Page<ManageFieldDTO> fields = fieldService.getAllFieldsWithFilters(name, departmentId, pageable);

        if (fields.isEmpty()) {
            return ResponseEntity.status(404).body(
                    new DataResponse<>("error", "Không tìm thấy lĩnh vực phù hợp")
            );
        }

        return ResponseEntity.ok(
                new DataResponse<>("success", "Lấy danh sách lĩnh vực thành công", fields)
        );
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/admin/field/create")
    public ResponseEntity<DataResponse<ManageFieldDTO>> createField(
            @RequestParam Integer departmentId,
            @RequestBody FieldRequest fieldRequest) {
        if (fieldRequest == null || fieldRequest.getName().trim().isEmpty()) {
            return ResponseEntity.status(400).body(
                    new DataResponse<>("error", "Dữ liệu lĩnh vực không hợp lệ")
            );
        }

        ManageFieldDTO savedField = fieldService.createField(departmentId, fieldRequest);

        return ResponseEntity.ok(
                new DataResponse<>("success", "Tạo lĩnh vực thành công", savedField)
        );
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @PutMapping("/admin/field/update")
    public ResponseEntity<DataResponse<ManageFieldDTO>> updateField(
            @RequestParam Integer id,
            @RequestParam Integer departmentId,
            @RequestBody FieldRequest fieldRequest) {
        if (fieldRequest == null || fieldRequest.getName().trim().isEmpty()) {
            return ResponseEntity.status(400).body(
                    new DataResponse<>("error", "Dữ liệu lĩnh vực không hợp lệ")
            );
        }

        ManageFieldDTO updatedField = fieldService.updateField(id, departmentId, fieldRequest);

        return ResponseEntity.ok(
                new DataResponse<>("success", "Cập nhật lĩnh vực thành công", updatedField)
        );
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @DeleteMapping("/admin/field/delete")
    public ResponseEntity<DataResponse<Void>> deleteField(@RequestParam Integer id) {
        try {
            fieldService.deleteFieldById(id);
            return ResponseEntity.ok(
                    new DataResponse<>("success", "Xóa lĩnh vực thành công")
            );
        } catch (Exception e) {
            return ResponseEntity.status(404).body(
                    new DataResponse<>("error", "Không tìm thấy lĩnh vực để xóa")
            );
        }
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/admin/field/detail")
    public ResponseEntity<DataResponse<ManageFieldDTO>> getFieldById(@RequestParam Integer id) {
        try {
            ManageFieldDTO manageFieldDTO = fieldService.getFieldById(id);
            return ResponseEntity.ok(
                    new DataResponse<>("success", "Lấy thông tin lĩnh vực thành công", manageFieldDTO)
            );
        } catch (Exception e) {
            return ResponseEntity.status(404).body(
                    new DataResponse<>("error", "Không tìm thấy lĩnh vực")
            );
        }
    }

    @PostMapping("/admin/export-fields-csv")
    public void exportFieldsToExcel(
            @RequestParam(required = false) String name,
            @RequestParam(required = false) Optional<String> departmentId,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "id") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir,
            HttpServletResponse response) throws IOException {

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

        Page<ManageFieldDTO> fieldPage = fieldService.getAllFieldsWithFilters(Optional.ofNullable(name), departmentId, pageable);
        List<ManageFieldDTO> fields = fieldPage.getContent();

        if (fields.isEmpty()) {
            throw new IOException("Không có lĩnh vực nào để xuất");
        }

        List<String> headers = List.of("Field ID", "Field Name", "Created At", "DepartmentId");
        List<List<String>> data = fields.stream()
                .map(field -> List.of(
                        field.getId().toString(),
                        field.getName(),
                        field.getCreatedAt().toString(),
                        field.getDepartmentId().toString()
                ))
                .collect(Collectors.toList());

        String fileName = "Fields_" + excelService.currentDate() + ".csv";

        excelService.generateExcelFile("Fields", headers, data, fileName, response);
    }

    @PostMapping("/admin/export-fields-pdf")
    public void exportFieldsToPdf(
            @RequestParam(required = false) String name,
            @RequestParam(required = false) Optional<String> departmentId,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "id") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir,
            HttpServletResponse response) throws DocumentException, IOException {

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

        Page<ManageFieldDTO> fieldPage = fieldService.getAllFieldsWithFilters(Optional.ofNullable(name), departmentId, pageable);
        List<ManageFieldDTO> fields = fieldPage.getContent();

        if (fields.isEmpty()) {
            throw new IOException("Không có lĩnh vực nào để xuất");
        }

        String templatePath = "/templates/field_template.html";
        String dataRows = buildFieldDataRows(fields);

        Map<String, String> placeholders = Map.of(
                "{{date}}", pdfService.currentDate(),
                "{{fields}}", dataRows,
                "{{logo_url}}", FilePaths.LOGO_URL
        );

        String fileName = "Fields_" + pdfService.currentDate() + ".pdf";
        String outputFilePath = FilePaths.PDF_OUTPUT_DIRECTORY + fileName;

        try (OutputStream fileOutputStream = new FileOutputStream(outputFilePath)) {
            pdfService.generatePdfFromTemplate(templatePath, placeholders, fileOutputStream);
        } catch (IOException | DocumentException e) {
            throw new IOException("Lỗi khi tạo hoặc lưu file PDF", e);
        }

        try (OutputStream responseStream = response.getOutputStream()) {
            pdfService.generatePdfFromTemplate(templatePath, placeholders, responseStream);
            response.flushBuffer();
        } catch (IOException | DocumentException e) {
            throw new IOException("Lỗi khi gửi file PDF qua HTTP response", e);
        }
    }


    private String buildFieldDataRows(List<ManageFieldDTO> fields) {
        StringBuilder dataRows = new StringBuilder();

        for (ManageFieldDTO field : fields) {
            dataRows.append("<tr>")
                    .append("<td>").append(field.getId()).append("</td>")
                    .append("<td>").append(field.getName()).append("</td>")
                    .append("<td>").append(field.getCreatedAt()).append("</td>")
                    .append("<td>").append(field.getDepartmentId()).append("</td>")
                    .append("</tr>");
        }

        return dataRows.toString();
    }


    @PostMapping("/admin/import-field-csv")
    public ResponseEntity<?> importFieldsFromCsv(@RequestParam("file") MultipartFile file) throws IOException {
        List<List<String>> csvData = excelService.importCsv(file);
        fieldService.importFields(csvData);

        return ResponseEntity.ok(DataResponse.builder()
                .status("success")
                .message("Import thành công.")
                .build());
    }
}

