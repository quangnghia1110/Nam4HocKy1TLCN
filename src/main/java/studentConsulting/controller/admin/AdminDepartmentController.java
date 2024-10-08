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
import studentConsulting.model.payload.dto.department_field.ManageDepartmentDTO;
import studentConsulting.model.payload.request.department_field.DepartmentRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.service.interfaces.admin.IAdminDepartmentService;
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
public class AdminDepartmentController {

    @Autowired
    private IAdminDepartmentService departmentService;

    @Autowired
    private ICommonExcelService excelService;

    @Autowired
    private ICommonPdfService pdfService;

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/admin/department/list")
    public ResponseEntity<DataResponse<Page<ManageDepartmentDTO>>> getDepartments(
            @RequestParam(required = false) String name,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "id") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir) {

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

        Page<ManageDepartmentDTO> departments = departmentService.getAllDepartmentsWithFilters(Optional.ofNullable(name), pageable);

        if (departments.isEmpty()) {
            return ResponseEntity.status(404).body(
                    new DataResponse<>("error", "Không tìm thấy phòng ban phù hợp")
            );
        }

        return ResponseEntity.ok(
                new DataResponse<>("success", "Lấy danh sách phòng ban thành công", departments)
        );
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/admin/department/create")
    public ResponseEntity<DataResponse<ManageDepartmentDTO>> createDepartment(@RequestBody DepartmentRequest departmentRequest) {
        if (departmentRequest == null || departmentRequest.getName().trim().isEmpty()) {
            return ResponseEntity.status(400).body(
                    new DataResponse<>("error", "Dữ liệu phòng ban không hợp lệ")
            );
        }

        ManageDepartmentDTO savedDepartment = departmentService.createDepartment(departmentRequest);

        return ResponseEntity.ok(
                new DataResponse<>("success", "Tạo phòng ban thành công", savedDepartment)
        );
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @PutMapping("/admin/department/update")
    public ResponseEntity<DataResponse<ManageDepartmentDTO>> updateDepartment(@RequestParam Integer id, @RequestBody DepartmentRequest departmentRequest) {
        if (departmentRequest == null || departmentRequest.getName().trim().isEmpty()) {
            return ResponseEntity.status(400).body(
                    new DataResponse<>("error", "Dữ liệu phòng ban không hợp lệ")
            );
        }

        ManageDepartmentDTO updatedDepartment = departmentService.updateDepartment(id, departmentRequest);

        return ResponseEntity.ok(
                new DataResponse<>("success", "Cập nhật phòng ban thành công", updatedDepartment)
        );
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @DeleteMapping("/admin/department/delete")
    public ResponseEntity<DataResponse<Void>> deleteDepartment(@RequestParam Integer id) {
        try {
            departmentService.deleteDepartmentById(id);
            return ResponseEntity.ok(
                    new DataResponse<>("success", "Xóa phòng ban thành công")
            );
        } catch (Exception e) {
            return ResponseEntity.status(404).body(
                    new DataResponse<>("error", "Không tìm thấy phòng ban để xóa")
            );
        }
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/admin/department/detail")
    public ResponseEntity<DataResponse<ManageDepartmentDTO>> getDepartmentById(@RequestParam Integer id) {
        try {
            ManageDepartmentDTO manageDepartmentDTO = departmentService.getDepartmentById(id);
            return ResponseEntity.ok(
                    new DataResponse<>("success", "Lấy thông tin phòng ban thành công", manageDepartmentDTO)
            );
        } catch (Exception e) {
            return ResponseEntity.status(404).body(
                    new DataResponse<>("error", "Không tìm thấy phòng ban")
            );
        }
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/admin/export-departments-csv")
    public void exportDepartmentsToExcel(
            @RequestParam(required = false) String name,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "id") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir,
            HttpServletResponse response) throws IOException {

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

        Page<ManageDepartmentDTO> departmentPage = departmentService.getAllDepartmentsWithFilters(Optional.ofNullable(name), pageable);
        List<ManageDepartmentDTO> departments = departmentPage.getContent();

        if (departments.isEmpty()) {
            throw new IOException("Không có phòng ban nào để xuất");
        }

        List<String> headers = List.of("Department ID", "Department Name", "Created At", "Description", "Logo");
        List<List<String>> data = departments.stream()
                .map(department -> List.of(
                        department.getId().toString(),
                        department.getName(),
                        department.getCreatedAt().toString(),
                        department.getDescription(),
                        department.getLogo()
                ))
                .collect(Collectors.toList());

        String fileName = "Departments_" + excelService.currentDate() + ".csv";

        excelService.generateExcelFile("Departments", headers, data, fileName, response);
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/admin/export-departments-pdf")
    public void exportDepartmentsToPdf(
            @RequestParam(required = false) String name,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "id") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir,
            HttpServletResponse response) throws DocumentException, IOException {

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

        Page<ManageDepartmentDTO> departmentPage = departmentService.getAllDepartmentsWithFilters(Optional.ofNullable(name), pageable);
        List<ManageDepartmentDTO> departments = departmentPage.getContent();

        if (departments.isEmpty()) {
            throw new IOException("Không có phòng ban nào để xuất");
        }

        String templatePath = "/templates/department_template.html";
        String dataRows = buildDepartmentDataRows(departments);

        Map<String, String> placeholders = Map.of(
                "{{date}}", pdfService.currentDate(),
                "{{departments}}", dataRows,
                "{{logo_url}}", FilePaths.LOGO_URL
        );

        String fileName = "Departments_" + pdfService.currentDate() + ".pdf";
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


    private String buildDepartmentDataRows(List<ManageDepartmentDTO> departments) {
        StringBuilder dataRows = new StringBuilder();

        for (ManageDepartmentDTO dept : departments) {
            dataRows.append("<tr>")
                    .append("<td>").append(dept.getId()).append("</td>")
                    .append("<td>").append(dept.getName()).append("</td>")
                    .append("<td>").append(dept.getCreatedAt()).append("</td>")
                    .append("<td>").append(dept.getDescription()).append("</td>")
                    .append("<td>").append(dept.getLogo()).append("</td>")
                    .append("</tr>");
        }

        return dataRows.toString();
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/admin/import-departments-csv")
    public ResponseEntity<?> importDepartmentsFromCsv(@RequestParam("file") MultipartFile file) throws IOException {
        List<List<String>> csvData = excelService.importCsv(file);
        departmentService.importDepartments(csvData);

        return ResponseEntity.ok(DataResponse.builder()
                .status("success")
                .message("Import thành công.")
                .build());
    }
}

