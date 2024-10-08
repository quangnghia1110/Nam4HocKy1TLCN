package studentConsulting.controller.admin;

import com.lowagie.text.DocumentException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;
import studentConsulting.constant.FilePaths;
import studentConsulting.constant.SecurityConstants;
import studentConsulting.model.payload.dto.authentication.ManageAccountDTO;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.service.interfaces.admin.IAdminAccountService;
import studentConsulting.service.interfaces.common.ICommonExcelService;
import studentConsulting.service.interfaces.common.ICommonPdfService;

import javax.servlet.http.HttpServletResponse;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.time.LocalDate;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

@RestController
@RequestMapping("${base.url}")
public class AdminAccountController {

    @Autowired
    private IAdminAccountService accountService;

    @Autowired
    private ICommonExcelService excelService;

    @Autowired
    private ICommonPdfService pdfService;

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/admin/account/list")
    public ResponseEntity<DataResponse<Page<ManageAccountDTO>>> getAccounts(
            @RequestParam(required = false) String email,
            @RequestParam(required = false) String username,
            @RequestParam(required = false) Optional<Boolean> isOnline,
            @RequestParam(required = false) Optional<Boolean> isActivity,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "createdAt") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir) {

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

        Page<ManageAccountDTO> accounts = accountService.getAllAccountsWithFilters(
                Optional.ofNullable(email),
                Optional.ofNullable(username),
                isOnline,
                Optional.ofNullable(startDate),
                Optional.ofNullable(endDate),
                isActivity,
                pageable);

        if (accounts.isEmpty()) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND).body(
                    DataResponse.<Page<ManageAccountDTO>>builder()
                            .status("error")
                            .message("Không tìm thấy tài khoản phù hợp.")
                            .build()
            );
        }

        return ResponseEntity.ok(
                DataResponse.<Page<ManageAccountDTO>>builder()
                        .status("success")
                        .message("Lấy danh sách tài khoản thành công")
                        .data(accounts)
                        .build()
        );
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/admin/account/detail")
    public ResponseEntity<DataResponse<ManageAccountDTO>> getAccountById(@RequestParam Integer id) {
        try {
            ManageAccountDTO account = accountService.getAccountById(id);
            return ResponseEntity.ok(
                    DataResponse.<ManageAccountDTO>builder()
                            .status("success")
                            .message("Lấy thông tin tài khoản thành công")
                            .data(account)
                            .build()
            );
        } catch (Exception e) {
            return ResponseEntity.status(404).body(
                    DataResponse.<ManageAccountDTO>builder()
                            .status("error")
                            .message("Không tìm thấy tài khoản với ID: " + id)
                            .build()
            );
        }
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @PutMapping("/admin/account/change-activity")
    public ResponseEntity<DataResponse<ManageAccountDTO>> changeAccountActivity(@RequestParam Integer id) {
        try {
            ManageAccountDTO updatedAccount = accountService.changeAccountActivity(id);
            return ResponseEntity.ok(
                    DataResponse.<ManageAccountDTO>builder()
                            .status("success")
                            .message("Cập nhật trạng thái tài khoản thành công")
                            .data(updatedAccount)
                            .build()
            );
        } catch (Exception e) {
            return ResponseEntity.status(404).body(
                    DataResponse.<ManageAccountDTO>builder()
                            .status("error")
                            .message("Không tìm thấy tài khoản với ID: " + id)
                            .build()
            );
        }
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/admin/export-account-csv")
    public void exportAccountsToExcel(
            @RequestParam(required = false) String email,
            @RequestParam(required = false) String username,
            @RequestParam(required = false) Optional<Boolean> isOnline,
            @RequestParam(required = false) Optional<Boolean> isActivity,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "createdAt") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir,
            HttpServletResponse response) throws IOException {

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

        Page<ManageAccountDTO> accountPage = accountService.getAllAccountsWithFilters(
                Optional.ofNullable(email),
                Optional.ofNullable(username),
                isOnline,
                Optional.ofNullable(startDate),
                Optional.ofNullable(endDate),
                isActivity,
                pageable);

        List<ManageAccountDTO> accounts = accountPage.getContent();

        if (accounts.isEmpty()) {
            throw new IOException("Không có tài khoản nào để xuất");
        }

        List<String> headers = List.of("User ID", "Username", "Email", "Is Online", "Is Active", "Created At", "Last Activity", "Department", "Role", "Role Consultant");
        List<List<String>> data = accounts.stream()
                .map(account -> List.of(
                        account.getId().toString(),
                        account.getUsername(),
                        account.getEmail(),
                        account.getIsOnline() != null ? account.getIsOnline().toString() : "N/A",
                        account.getIsActivity() ? "Có" : "Không",
                        account.getCreatedAt().toString(),
                        account.getLastActivity() != null ? account.getLastActivity().toString() : "N/A",
                        account.getDepartment() != null ? account.getDepartment().getName() : "N/A",
                        account.getRole() != null ? account.getRole().getName() : "N/A",
                        account.getRoleConsultant() != null ? account.getRoleConsultant().getName() : "N/A"
                ))
                .collect(Collectors.toList());

        String fileName = "Accounts_" + excelService.currentDate() + ".csv";
        excelService.generateExcelFile("Accounts", headers, data, fileName, response);
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/admin/export-account-pdf")
    public void exportAccountsToPdf(
            @RequestParam(required = false) String email,
            @RequestParam(required = false) String username,
            @RequestParam(required = false) Optional<Boolean> isOnline,
            @RequestParam(required = false) Optional<Boolean> isActivity,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "createdAt") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir,
            HttpServletResponse response) throws IOException {

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

        Page<ManageAccountDTO> accountPage = accountService.getAllAccountsWithFilters(
                Optional.ofNullable(email),
                Optional.ofNullable(username),
                isOnline,
                Optional.ofNullable(startDate),
                Optional.ofNullable(endDate),
                isActivity,
                pageable);

        List<ManageAccountDTO> accounts = accountPage.getContent();

        if (accounts.isEmpty()) {
            throw new IOException("Không có tài khoản nào để xuất");
        }

        String dataRows = buildAccountDataRows(accounts);

        String templatePath = "/templates/account_template.html";
        Map<String, String> placeholders = Map.of(
                "{{date}}", pdfService.currentDate(),
                "{{accounts}}", dataRows,
                "{{logo_url}}", FilePaths.LOGO_URL
        );

        String fileName = "Accounts_" + pdfService.currentDate() + ".pdf";
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

    private String buildAccountDataRows(List<ManageAccountDTO> accounts) {
        StringBuilder dataRows = new StringBuilder();

        for (ManageAccountDTO account : accounts) {
            dataRows.append("<tr>")
                    .append("<td>").append(account.getId()).append("</td>")
                    .append("<td>").append(account.getUsername()).append("</td>")
                    .append("<td>").append(account.getEmail()).append("</td>")
                    .append("<td>").append(account.getIsOnline() != null ? account.getIsOnline() : "N/A").append("</td>")
                    .append("<td>").append(account.getIsActivity() ? "Có" : "Không").append("</td>")
                    .append("<td>").append(account.getCreatedAt()).append("</td>")
                    .append("<td>").append(account.getLastActivity() != null ? account.getLastActivity() : "N/A").append("</td>")
                    .append("<td>").append(account.getDepartment() != null ? account.getDepartment().getName() : "N/A").append("</td>")
                    .append("<td>").append(account.getRole() != null ? account.getRole().getName() : "N/A").append("</td>")
                    .append("<td>").append(account.getRoleConsultant() != null ? account.getRoleConsultant().getName() : "N/A").append("</td>")
                    .append("</tr>");
        }

        return dataRows.toString();
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/admin/import-account-csv")
    public ResponseEntity<?> importAccountsFromCsv(@RequestParam("file") MultipartFile file) throws IOException {
        List<List<String>> csvData = excelService.importCsv(file);
        accountService.importAccounts(csvData);

        return ResponseEntity.ok(DataResponse.builder()
                .status("success")
                .message("Import thành công.")
                .build());
    }

}
