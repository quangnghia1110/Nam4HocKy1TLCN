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
import studentConsulting.model.payload.dto.user.ManageRoleConsultantDTO;
import studentConsulting.model.payload.request.authentication.RoleConsultantRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.service.interfaces.admin.IAdminRoleConsultantService;
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
public class AdminRoleConsultantController {

    @Autowired
    private IAdminRoleConsultantService roleConsultantService;

    @Autowired
    private ICommonExcelService excelService;

    @Autowired
    private ICommonPdfService pdfService;

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/admin/role-consultant/list")
    public ResponseEntity<DataResponse<Page<ManageRoleConsultantDTO>>> getRoleConsultants(
            @RequestParam(required = false) String name,
            @RequestParam(required = false) Integer roleId, // thêm roleId vào request
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "id") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir) {

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

        Page<ManageRoleConsultantDTO> roleConsultants = roleConsultantService.getAllRoleConsultantsWithFilters(
                Optional.ofNullable(name),
                Optional.ofNullable(roleId),
                pageable);

        if (roleConsultants.isEmpty()) {
            return ResponseEntity.status(404).body(
                    DataResponse.<Page<ManageRoleConsultantDTO>>builder()
                            .status("error")
                            .message("Không tìm thấy role consultant phù hợp")
                            .build()
            );
        }

        return ResponseEntity.ok(
                DataResponse.<Page<ManageRoleConsultantDTO>>builder()
                        .status("success")
                        .message("Lấy danh sách role consultant thành công")
                        .data(roleConsultants)
                        .build()
        );
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/admin/role-consultant/create")
    public ResponseEntity<DataResponse<ManageRoleConsultantDTO>> createRoleConsultant(
            @RequestParam Integer roleId,
            @RequestBody RoleConsultantRequest roleConsultantRequest) {
        if (roleConsultantRequest == null || roleConsultantRequest.getName().trim().isEmpty()) {
            return ResponseEntity.status(400).body(
                    DataResponse.<ManageRoleConsultantDTO>builder()
                            .status("error")
                            .message("Dữ liệu role consultant không hợp lệ")
                            .build()
            );
        }

        ManageRoleConsultantDTO savedRoleConsultant = roleConsultantService.createRoleConsultant(roleId, roleConsultantRequest);

        return ResponseEntity.ok(
                DataResponse.<ManageRoleConsultantDTO>builder()
                        .status("success")
                        .message("Tạo role consultant thành công")
                        .data(savedRoleConsultant)
                        .build()
        );
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @PutMapping("/admin/role-consultant/update")
    public ResponseEntity<DataResponse<ManageRoleConsultantDTO>> updateRoleConsultant(
            @RequestParam Integer id,
            @RequestParam Integer roleId,
            @RequestBody RoleConsultantRequest roleConsultantRequest) {
        if (roleConsultantRequest == null || roleConsultantRequest.getName().trim().isEmpty()) {
            return ResponseEntity.status(400).body(
                    DataResponse.<ManageRoleConsultantDTO>builder()
                            .status("error")
                            .message("Dữ liệu role consultant không hợp lệ")
                            .build()
            );
        }

        ManageRoleConsultantDTO updatedRoleConsultant = roleConsultantService.updateRoleConsultant(id, roleId, roleConsultantRequest);

        return ResponseEntity.ok(
                DataResponse.<ManageRoleConsultantDTO>builder()
                        .status("success")
                        .message("Cập nhật role consultant thành công")
                        .data(updatedRoleConsultant)
                        .build()
        );
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @DeleteMapping("/admin/role-consultant/delete")
    public ResponseEntity<DataResponse<Void>> deleteRoleConsultant(@RequestParam Integer id) {
        try {
            roleConsultantService.deleteRoleConsultantById(id);
            return ResponseEntity.ok(
                    DataResponse.<Void>builder()
                            .status("success")
                            .message("Xóa role consultant thành công")
                            .build()
            );
        } catch (Exception e) {
            return ResponseEntity.status(404).body(
                    DataResponse.<Void>builder()
                            .status("error")
                            .message("Không tìm thấy role consultant để xóa")
                            .build()
            );
        }
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/admin/role-consultant/detail")
    public ResponseEntity<DataResponse<ManageRoleConsultantDTO>> getRoleConsultantById(@RequestParam Integer id) {
        try {
            ManageRoleConsultantDTO manageRoleConsultantDTO = roleConsultantService.getRoleConsultantById(id);
            return ResponseEntity.ok(
                    DataResponse.<ManageRoleConsultantDTO>builder()
                            .status("success")
                            .message("Lấy thông tin role consultant thành công")
                            .data(manageRoleConsultantDTO)
                            .build()
            );
        } catch (Exception e) {
            return ResponseEntity.status(404).body(
                    DataResponse.<ManageRoleConsultantDTO>builder()
                            .status("error")
                            .message("Không tìm thấy role consultant")
                            .build()
            );
        }
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/admin/export-role-consultant-csv")
    public void exportRoleConsultantsToExcel(
            @RequestParam(required = false) String name,
            @RequestParam(required = false) Integer roleId,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "id") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir,
            HttpServletResponse response) throws IOException {

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

        Page<ManageRoleConsultantDTO> roleConsultantPage = roleConsultantService.getAllRoleConsultantsWithFilters(
                Optional.ofNullable(name),
                Optional.ofNullable(roleId),
                pageable
        );
        List<ManageRoleConsultantDTO> roleConsultants = roleConsultantPage.getContent();

        if (roleConsultants.isEmpty()) {
            throw new IOException("Không có role consultant nào để xuất");
        }

        List<String> headers = List.of("Role Consultant ID", "Name", "Role ID", "Created At");
        List<List<String>> data = roleConsultants.stream()
                .map(roleConsultant -> List.of(
                        roleConsultant.getId().toString(),
                        roleConsultant.getName(),
                        roleConsultant.getRoleId().toString(),
                        roleConsultant.getCreatedAt().toString()
                ))
                .collect(Collectors.toList());

        String fileName = "RoleConsultants_" + excelService.currentDate() + ".csv";

        excelService.generateExcelFile("RoleConsultants", headers, data, fileName, response);
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/admin/export-role-consultant-pdf")
    public void exportRoleConsultantsToPdf(
            @RequestParam(required = false) String name,
            @RequestParam(required = false) Integer roleId,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "id") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir,
            HttpServletResponse response) throws DocumentException, IOException {

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

        Page<ManageRoleConsultantDTO> roleConsultantPage = roleConsultantService.getAllRoleConsultantsWithFilters(
                Optional.ofNullable(name),
                Optional.ofNullable(roleId),
                pageable
        );
        List<ManageRoleConsultantDTO> roleConsultants = roleConsultantPage.getContent();

        if (roleConsultants.isEmpty()) {
            throw new IOException("Không có role consultant nào để xuất");
        }

        String templatePath = "/templates/role_consultant_template.html";
        String dataRows = buildRoleConsultantDataRows(roleConsultants);

        Map<String, String> placeholders = Map.of(
                "{{date}}", pdfService.currentDate(),
                "{{role_consultants}}", dataRows,
                "{{logo_url}}", FilePaths.LOGO_URL
        );

        String fileName = "RoleConsultants_" + pdfService.currentDate() + ".pdf";
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

    private String buildRoleConsultantDataRows(List<ManageRoleConsultantDTO> roleConsultants) {
        StringBuilder dataRows = new StringBuilder();

        for (ManageRoleConsultantDTO roleConsultant : roleConsultants) {
            dataRows.append("<tr>")
                    .append("<td>").append(roleConsultant.getId()).append("</td>")
                    .append("<td>").append(roleConsultant.getName()).append("</td>")
                    .append("<td>").append(roleConsultant.getRoleId()).append("</td>")
                    .append("<td>").append(roleConsultant.getCreatedAt()).append("</td>")
                    .append("</tr>");
        }

        return dataRows.toString();
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/admin/import-role-consultant-csv")
    public ResponseEntity<?> importRoleConsultantsFromCsv(@RequestParam("file") MultipartFile file) throws IOException {
        List<List<String>> csvData = excelService.importCsv(file);
        roleConsultantService.importRoleConsultants(csvData);

        return ResponseEntity.ok(DataResponse.builder()
                .status("success")
                .message("Import thành công.")
                .build());
    }
}
