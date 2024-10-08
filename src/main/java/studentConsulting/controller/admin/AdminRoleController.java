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
import studentConsulting.model.payload.dto.authentication.RoleDTO;
import studentConsulting.model.payload.request.authentication.RoleRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.service.interfaces.admin.IAdminRoleService;
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
public class AdminRoleController {

    @Autowired
    private IAdminRoleService roleService;

    @Autowired
    private ICommonExcelService excelService;

    @Autowired
    private ICommonPdfService pdfService;

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/admin/role/list")
    public ResponseEntity<DataResponse<Page<RoleDTO>>> getRoles(
            @RequestParam(required = false) String name,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "id") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir) {

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

        Page<RoleDTO> roles = roleService.getAllRolesWithFilters(Optional.ofNullable(name), pageable);

        if (roles.isEmpty()) {
            return ResponseEntity.status(404).body(
                    DataResponse.<Page<RoleDTO>>builder()
                            .status("error")
                            .message("Không tìm thấy vai trò phù hợp")
                            .build()
            );
        }

        return ResponseEntity.ok(
                DataResponse.<Page<RoleDTO>>builder()
                        .status("success")
                        .message("Lấy danh sách vai trò thành công")
                        .data(roles)
                        .build()
        );
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/admin/role/create")
    public ResponseEntity<DataResponse<RoleDTO>> createRole(@RequestBody RoleRequest roleRequest) {
        if (roleRequest == null || roleRequest.getName().trim().isEmpty()) {
            return ResponseEntity.status(400).body(
                    DataResponse.<RoleDTO>builder()
                            .status("error")
                            .message("Dữ liệu vai trò không hợp lệ")
                            .build()
            );
        }

        RoleDTO savedRole = roleService.createRole(roleRequest);

        return ResponseEntity.ok(
                DataResponse.<RoleDTO>builder()
                        .status("success")
                        .message("Tạo vai trò thành công")
                        .data(savedRole)
                        .build()
        );
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @PutMapping("/admin/role/update")
    public ResponseEntity<DataResponse<RoleDTO>> updateRole(@RequestParam Integer id, @RequestBody RoleRequest roleRequest) {
        if (roleRequest == null || roleRequest.getName().trim().isEmpty()) {
            return ResponseEntity.status(400).body(
                    DataResponse.<RoleDTO>builder()
                            .status("error")
                            .message("Dữ liệu vai trò không hợp lệ")
                            .build()
            );
        }

        RoleDTO updatedRole = roleService.updateRole(id, roleRequest);

        return ResponseEntity.ok(
                DataResponse.<RoleDTO>builder()
                        .status("success")
                        .message("Cập nhật vai trò thành công")
                        .data(updatedRole)
                        .build()
        );
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @DeleteMapping("/admin/role/delete")
    public ResponseEntity<DataResponse<Void>> deleteRole(@RequestParam Integer id) {
        try {
            roleService.deleteRoleById(id);
            return ResponseEntity.ok(
                    DataResponse.<Void>builder()
                            .status("success")
                            .message("Xóa vai trò thành công")
                            .build()
            );
        } catch (Exception e) {
            return ResponseEntity.status(404).body(
                    DataResponse.<Void>builder()
                            .status("error")
                            .message("Không tìm thấy vai trò để xóa")
                            .build()
            );
        }
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/admin/role/detail")
    public ResponseEntity<DataResponse<RoleDTO>> getRoleById(@RequestParam Integer id) {
        try {
            RoleDTO RoleDTO = roleService.getRoleById(id);
            return ResponseEntity.ok(
                    DataResponse.<RoleDTO>builder()
                            .status("success")
                            .message("Lấy thông tin vai trò thành công")
                            .data(RoleDTO)
                            .build()
            );
        } catch (Exception e) {
            return ResponseEntity.status(404).body(
                    DataResponse.<RoleDTO>builder()
                            .status("error")
                            .message("Không tìm thấy vai trò")
                            .build()
            );
        }
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/admin/export-role-csv")
    public void exportRolesToExcel(
            @RequestParam(required = false) String name,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "id") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir,
            HttpServletResponse response) throws IOException {

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

        Page<RoleDTO> rolePage = roleService.getAllRolesWithFilters(Optional.ofNullable(name), pageable);
        List<RoleDTO> roles = rolePage.getContent();

        if (roles.isEmpty()) {
            throw new IOException("Không có vai trò nào để xuất");
        }

        List<String> headers = List.of("Role ID", "Role Name");
        List<List<String>> data = roles.stream()
                .map(role -> List.of(
                        role.getId().toString(),
                        role.getName()
                ))
                .collect(Collectors.toList());

        String fileName = "Roles_" + excelService.currentDate() + ".csv";

        excelService.generateExcelFile("Roles", headers, data, fileName, response);
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/admin/export-role-pdf")
    public void exportRolesToPdf(
            @RequestParam(required = false) String name,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "id") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir,
            HttpServletResponse response) throws DocumentException, IOException {

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

        Page<RoleDTO> rolePage = roleService.getAllRolesWithFilters(Optional.ofNullable(name), pageable);
        List<RoleDTO> roles = rolePage.getContent();

        if (roles.isEmpty()) {
            throw new IOException("Không có vai trò nào để xuất");
        }

        String templatePath = "/templates/role_template.html";
        String dataRows = buildRoleDataRows(roles);

        Map<String, String> placeholders = Map.of(
                "{{date}}", pdfService.currentDate(),
                "{{roles}}", dataRows,
                "{{logo_url}}", FilePaths.LOGO_URL
        );

        String fileName = "Roles_" + pdfService.currentDate() + ".pdf";
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

    private String buildRoleDataRows(List<RoleDTO> roles) {
        StringBuilder dataRows = new StringBuilder();

        for (RoleDTO role : roles) {
            dataRows.append("<tr>")
                    .append("<td>").append(role.getId()).append("</td>")
                    .append("<td>").append(role.getName()).append("</td>")
                    .append("</tr>");
        }

        return dataRows.toString();
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/admin/import-role-csv")
    public ResponseEntity<?> importRolesFromCsv(@RequestParam("file") MultipartFile file) throws IOException {
        List<List<String>> csvData = excelService.importCsv(file);
        roleService.importRoles(csvData);

        return ResponseEntity.ok(DataResponse.builder()
                .status("success")
                .message("Import thành công.")
                .build());
    }
}
