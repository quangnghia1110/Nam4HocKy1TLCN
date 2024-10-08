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
import studentConsulting.model.payload.dto.user.ManageRoleAskDTO;
import studentConsulting.model.payload.request.authentication.RoleAskRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.service.interfaces.admin.IAdminRoleAskService;
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
public class AdminRoleAskController {

    @Autowired
    private IAdminRoleAskService roleAskService;

    @Autowired
    private ICommonExcelService excelService;

    @Autowired
    private ICommonPdfService pdfService;

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/admin/role-ask/list")
    public ResponseEntity<DataResponse<Page<ManageRoleAskDTO>>> getRoleAsks(
            @RequestParam(required = false) String name,
            @RequestParam(required = false) Integer roleId, // Thêm roleId vào request
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "id") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir) {

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

        Page<ManageRoleAskDTO> roleAsks = roleAskService.getAllRoleAsksWithFilters(
                Optional.ofNullable(name),
                Optional.ofNullable(roleId),
                pageable
        );

        if (roleAsks.isEmpty()) {
            return ResponseEntity.status(404).body(
                    DataResponse.<Page<ManageRoleAskDTO>>builder()
                            .status("error")
                            .message("Không tìm thấy role ask phù hợp")
                            .build()
            );
        }

        return ResponseEntity.ok(
                DataResponse.<Page<ManageRoleAskDTO>>builder()
                        .status("success")
                        .message("Lấy danh sách role ask thành công")
                        .data(roleAsks)
                        .build()
        );
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/admin/role-ask/create")
    public ResponseEntity<DataResponse<ManageRoleAskDTO>> createRoleAsk(
            @RequestParam Integer roleId,
            @RequestBody RoleAskRequest roleAskRequest) {

        if (roleId != 4) {
            return ResponseEntity.status(400).body(
                    DataResponse.<ManageRoleAskDTO>builder()
                            .status("error")
                            .message("Role này không có liên kết role ask")
                            .build()
            );
        }

        if (roleAskRequest == null || roleAskRequest.getName().trim().isEmpty()) {
            return ResponseEntity.status(400).body(
                    DataResponse.<ManageRoleAskDTO>builder()
                            .status("error")
                            .message("Dữ liệu role ask không hợp lệ")
                            .build()
            );
        }

        ManageRoleAskDTO savedRoleAsk = roleAskService.createRoleAsk(roleId, roleAskRequest);

        return ResponseEntity.ok(
                DataResponse.<ManageRoleAskDTO>builder()
                        .status("success")
                        .message("Tạo role ask thành công")
                        .data(savedRoleAsk)
                        .build()
        );
    }


    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @PutMapping("/admin/role-ask/update")
    public ResponseEntity<DataResponse<ManageRoleAskDTO>> updateRoleAsk(
            @RequestParam Integer id,
            @RequestParam Integer roleId,
            @RequestBody RoleAskRequest roleAskRequest) {
        if (roleAskRequest == null || roleAskRequest.getName().trim().isEmpty()) {
            return ResponseEntity.status(400).body(
                    DataResponse.<ManageRoleAskDTO>builder()
                            .status("error")
                            .message("Dữ liệu role ask không hợp lệ")
                            .build()
            );
        }

        ManageRoleAskDTO updatedRoleAsk = roleAskService.updateRoleAsk(id, roleId, roleAskRequest);

        return ResponseEntity.ok(
                DataResponse.<ManageRoleAskDTO>builder()
                        .status("success")
                        .message("Cập nhật role ask thành công")
                        .data(updatedRoleAsk)
                        .build()
        );
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @DeleteMapping("/admin/role-ask/delete")
    public ResponseEntity<DataResponse<Void>> deleteRoleAsk(@RequestParam Integer id) {
        try {
            roleAskService.deleteRoleAskById(id);
            return ResponseEntity.ok(
                    DataResponse.<Void>builder()
                            .status("success")
                            .message("Xóa role ask thành công")
                            .build()
            );
        } catch (Exception e) {
            return ResponseEntity.status(404).body(
                    DataResponse.<Void>builder()
                            .status("error")
                            .message("Không tìm thấy role ask để xóa")
                            .build()
            );
        }
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/admin/role-ask/detail")
    public ResponseEntity<DataResponse<ManageRoleAskDTO>> getRoleAskById(@RequestParam Integer id) {
        try {
            ManageRoleAskDTO manageRoleAskDTO = roleAskService.getRoleAskById(id);
            return ResponseEntity.ok(
                    DataResponse.<ManageRoleAskDTO>builder()
                            .status("success")
                            .message("Lấy thông tin role ask thành công")
                            .data(manageRoleAskDTO)
                            .build()
            );
        } catch (Exception e) {
            return ResponseEntity.status(404).body(
                    DataResponse.<ManageRoleAskDTO>builder()
                            .status("error")
                            .message("Không tìm thấy role ask")
                            .build()
            );
        }
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/admin/export-role-ask-csv")
    public void exportRoleAsksToExcel(
            @RequestParam(required = false) String name,
            @RequestParam(required = false) Integer roleId,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "id") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir,
            HttpServletResponse response) throws IOException {

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

        Page<ManageRoleAskDTO> roleAskPage = roleAskService.getAllRoleAsksWithFilters(
                Optional.ofNullable(name),
                Optional.ofNullable(roleId),
                pageable
        );
        List<ManageRoleAskDTO> roleAsks = roleAskPage.getContent();

        if (roleAsks.isEmpty()) {
            throw new IOException("Không có role ask nào để xuất");
        }

        List<String> headers = List.of("Role Ask ID", "Name", "Role ID", "Created At");
        List<List<String>> data = roleAsks.stream()
                .map(roleAsk -> List.of(
                        roleAsk.getId().toString(),
                        roleAsk.getName(),
                        roleAsk.getRoleId().toString(),
                        roleAsk.getCreatedAt().toString()
                ))
                .collect(Collectors.toList());

        String fileName = "RoleAsks_" + excelService.currentDate() + ".csv";

        excelService.generateExcelFile("RoleAsks", headers, data, fileName, response);
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/admin/export-role-ask-pdf")
    public void exportRoleAsksToPdf(
            @RequestParam(required = false) String name,
            @RequestParam(required = false) Integer roleId,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "id") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir,
            HttpServletResponse response) throws DocumentException, IOException {

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

        Page<ManageRoleAskDTO> roleAskPage = roleAskService.getAllRoleAsksWithFilters(
                Optional.ofNullable(name),
                Optional.ofNullable(roleId),
                pageable
        );
        List<ManageRoleAskDTO> roleAsks = roleAskPage.getContent();

        if (roleAsks.isEmpty()) {
            throw new IOException("Không có role ask nào để xuất");
        }

        String templatePath = "/templates/role_ask_template.html";
        String dataRows = buildRoleAskDataRows(roleAsks);

        Map<String, String> placeholders = Map.of(
                "{{date}}", pdfService.currentDate(),
                "{{role_asks}}", dataRows,
                "{{logo_url}}", FilePaths.LOGO_URL
        );

        String fileName = "RoleAsks_" + pdfService.currentDate() + ".pdf";
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

    private String buildRoleAskDataRows(List<ManageRoleAskDTO> roleAsks) {
        StringBuilder dataRows = new StringBuilder();

        for (ManageRoleAskDTO roleAsk : roleAsks) {
            dataRows.append("<tr>")
                    .append("<td>").append(roleAsk.getId()).append("</td>")
                    .append("<td>").append(roleAsk.getName()).append("</td>")
                    .append("<td>").append(roleAsk.getRoleId()).append("</td>")
                    .append("<td>").append(roleAsk.getCreatedAt()).append("</td>")
                    .append("</tr>");
        }

        return dataRows.toString();
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/admin/import-role-ask-csv")
    public ResponseEntity<?> importRoleAsksFromCsv(@RequestParam("file") MultipartFile file) throws IOException {
        List<List<String>> csvData = excelService.importCsv(file);
        roleAskService.importRoleAsks(csvData);

        return ResponseEntity.ok(DataResponse.builder()
                .status("success")
                .message("Import thành công.")
                .build());
    }

}
