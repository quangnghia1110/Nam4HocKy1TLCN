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
import studentConsulting.model.payload.dto.user.ManageUserDTO;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.service.interfaces.admin.IAdminUserInformationService;
import studentConsulting.service.interfaces.common.ICommonExcelService;
import studentConsulting.service.interfaces.common.ICommonPdfService;

import javax.servlet.http.HttpServletResponse;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.time.LocalDate;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

@RestController
@RequestMapping("${base.url}")
public class AdminUserInformationController {
    @Autowired
    private IAdminUserInformationService userInformationService;

    @Autowired
    private ICommonExcelService excelService;

    @Autowired
    private ICommonPdfService pdfService;

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/admin/user-information/list")
    public ResponseEntity<DataResponse<Page<ManageUserDTO>>> getUsers(
            @RequestParam(required = false) String name,
            @RequestParam(required = false) String studentCode,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "createdAt") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir) {

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

        Page<ManageUserDTO> users = userInformationService.getAllUsersWithFilters(
                Optional.ofNullable(name),
                Optional.ofNullable(studentCode),
                Optional.ofNullable(startDate),
                Optional.ofNullable(endDate),
                pageable);

        if (users.isEmpty()) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND).body(
                    DataResponse.<Page<ManageUserDTO>>builder()
                            .status("error")
                            .message("Không tìm thấy người dùng phù hợp.")
                            .build()
            );
        }

        return ResponseEntity.ok(
                DataResponse.<Page<ManageUserDTO>>builder()
                        .status("success")
                        .message("Lấy danh sách người dùng thành công")
                        .data(users)
                        .build()
        );
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/admin/user-information/detail")
    public ResponseEntity<DataResponse<ManageUserDTO>> getUserById(@RequestParam Integer id) {
        try {
            ManageUserDTO userInformation = userInformationService.getUserById(id);
            return ResponseEntity.ok(
                    DataResponse.<ManageUserDTO>builder()
                            .status("success")
                            .message("Lấy thông tin người dùng thành công")
                            .data(userInformation)
                            .build()
            );
        } catch (Exception e) {
            return ResponseEntity.status(404).body(
                    DataResponse.<ManageUserDTO>builder()
                            .status("error")
                            .message("Không tìm thấy người dùng với ID: " + id)
                            .build()
            );
        }
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/admin/export-user-information-csv")
    public void exportUsersToExcel(
            @RequestParam(required = false) String name,
            @RequestParam(required = false) String studentCode,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "createdAt") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir,
            HttpServletResponse response) throws IOException {

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

        Page<ManageUserDTO> userPage = userInformationService.getAllUsersWithFilters(
                Optional.ofNullable(name),
                Optional.ofNullable(studentCode),
                Optional.ofNullable(startDate),
                Optional.ofNullable(endDate),
                pageable);
        List<ManageUserDTO> users = userPage.getContent();

        if (users.isEmpty()) {
            throw new IOException("Không có người dùng nào để xuất");
        }

        List<String> headers = List.of("User ID", "First Name", "Last Name", "Student Code", "Gender", "Phone", "Email", "Created At", "Address Line", "Province", "District", "Ward");
        List<List<String>> data = users.stream()
                .map(user -> List.of(
                        user.getId().toString(),
                        user.getFirstName(),
                        user.getLastName(),
                        user.getStudentCode(),
                        user.getGender(),
                        user.getPhone(),
                        user.getSchoolName(),
                        user.getCreatedAt().toString(),
                        user.getAddress().getLine(),
                        user.getAddress().getProvinceFullName(),
                        user.getAddress().getDistrictFullName(),
                        user.getAddress().getWardFullName()
                ))
                .collect(Collectors.toList());

        String fileName = "Users_" + excelService.currentDate() + ".csv";
        excelService.generateExcelFile("Users", headers, data, fileName, response);
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/admin/export-user-information-pdf")
    public void exportUsersToPdf(
            @RequestParam(required = false) String name,
            @RequestParam(required = false) String studentCode,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "createdAt") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir,
            HttpServletResponse response) throws DocumentException, IOException {

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

        Page<ManageUserDTO> userPage = userInformationService.getAllUsersWithFilters(
                Optional.ofNullable(name),
                Optional.ofNullable(studentCode),
                Optional.ofNullable(startDate),
                Optional.ofNullable(endDate),
                pageable);
        List<ManageUserDTO> users = userPage.getContent();

        if (users.isEmpty()) {
            throw new IOException("Không có người dùng nào để xuất");
        }

        List<Map<String, String>> userData = users.stream().map(user -> {
            Map<String, String> userMap = new HashMap<>();
            userMap.put("userId", user.getId().toString());
            userMap.put("firstName", user.getFirstName());
            userMap.put("lastName", user.getLastName());
            userMap.put("studentCode", user.getStudentCode());
            userMap.put("gender", user.getGender());
            userMap.put("phone", user.getPhone());
            userMap.put("email", user.getSchoolName());
            userMap.put("createdAt", user.getCreatedAt().toString());
            userMap.put("addressLine", user.getAddress().getLine());
            userMap.put("province", user.getAddress().getProvinceFullName());
            userMap.put("district", user.getAddress().getDistrictFullName());
            userMap.put("ward", user.getAddress().getWardFullName());
            return userMap;
        }).collect(Collectors.toList());


        String dataRows = buildUserDataRows(userData);

        String templatePath = "/templates/user_template.html";
        Map<String, String> placeholders = Map.of(
                "{{date}}", pdfService.currentDate(),
                "{{users}}", dataRows,
                "{{logo_url}}", FilePaths.LOGO_URL
        );

        String fileName = "Users_" + pdfService.currentDate() + ".pdf";
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


    private String buildUserDataRows(List<Map<String, String>> userData) {
        StringBuilder dataRows = new StringBuilder();

        for (Map<String, String> user : userData) {
            dataRows.append("<tr>")
                    .append("<td>").append(user.get("userId")).append("</td>")
                    .append("<td>").append(user.get("firstName")).append("</td>")
                    .append("<td>").append(user.get("lastName")).append("</td>")
                    .append("<td>").append(user.get("studentCode")).append("</td>")
                    .append("<td>").append(user.get("gender")).append("</td>")
                    .append("<td>").append(user.get("phone")).append("</td>")
                    .append("<td>").append(user.get("email")).append("</td>")
                    .append("<td>").append(user.get("createdAt")).append("</td>")
                    .append("<td>").append(user.get("addressLine")).append("</td>")
                    .append("<td>").append(user.get("province")).append("</td>")
                    .append("<td>").append(user.get("district")).append("</td>")
                    .append("<td>").append(user.get("ward")).append("</td>")
                    .append("</tr>");
        }

        return dataRows.toString();
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/admin/import-user-information-csv")
    public ResponseEntity<?> importUsersFromCsv(@RequestParam("file") MultipartFile file) throws IOException {
        List<List<String>> csvData = excelService.importCsv(file);
        userInformationService.importUsers(csvData);

        return ResponseEntity.ok(DataResponse.builder()
                .status("success")
                .message("Import thành công.")
                .build());
    }
}
