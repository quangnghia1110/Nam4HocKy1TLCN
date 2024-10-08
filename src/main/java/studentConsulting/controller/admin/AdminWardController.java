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
import studentConsulting.model.payload.dto.address.ManageWardDTO;
import studentConsulting.model.payload.request.address.WardRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.service.interfaces.admin.IAdminWardService;
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
public class AdminWardController {

    @Autowired
    private IAdminWardService wardService;

    @Autowired
    private ICommonExcelService excelService;

    @Autowired
    private ICommonPdfService pdfService;

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/admin/ward/list")
    public ResponseEntity<DataResponse<Page<ManageWardDTO>>> getWards(
            @RequestParam String districtCode,
            @RequestParam(required = false) Optional<String> code,
            @RequestParam(required = false) Optional<String> name,
            @RequestParam(required = false) Optional<String> nameEn,
            @RequestParam(required = false) Optional<String> fullName,
            @RequestParam(required = false) Optional<String> fullNameEn,
            @RequestParam(required = false) Optional<String> codeName,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "code") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir) {

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));
        Page<ManageWardDTO> wards = wardService.getAllWardsWithFilters(code, name, nameEn, fullName, fullNameEn, codeName, Optional.of(districtCode), pageable);

        if (wards.isEmpty()) {
            return ResponseEntity.status(404).body(
                    DataResponse.<Page<ManageWardDTO>>builder()
                            .status("error")
                            .message("Không tìm thấy Phường/Xã phù hợp trong quận/huyện")
                            .build()
            );
        }

        return ResponseEntity.ok(
                DataResponse.<Page<ManageWardDTO>>builder()
                        .status("success")
                        .message("Lấy danh sách Phường/Xã thành công")
                        .data(wards)
                        .build()
        );
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/admin/ward/create")
    public ResponseEntity<DataResponse<ManageWardDTO>> createWard(
            @RequestParam String districtCode,
            @RequestParam String code,
            @RequestBody WardRequest wardRequest) {
        if (wardRequest == null || districtCode == null || code == null) {
            return ResponseEntity.status(400).body(
                    DataResponse.<ManageWardDTO>builder()
                            .status("error")
                            .message("Dữ liệu Phường/Xã hoặc mã quận/huyện không hợp lệ")
                            .build()
            );
        }

        if (wardService.existsByCode(code)) {
            return ResponseEntity.status(400).body(
                    DataResponse.<ManageWardDTO>builder()
                            .status("error")
                            .message("Mã Phường/Xã đã tồn tại")
                            .build()
            );
        }

        ManageWardDTO savedWard = wardService.createWard(code, districtCode, wardRequest);

        return ResponseEntity.ok(
                DataResponse.<ManageWardDTO>builder()
                        .status("success")
                        .message("Tạo Phường/Xã thành công")
                        .data(savedWard)
                        .build()
        );
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @PutMapping("/admin/ward/update")
    public ResponseEntity<DataResponse<ManageWardDTO>> updateWard(
            @RequestParam String code,
            @RequestParam String districtCode,
            @RequestBody WardRequest wardRequest) {
        if (wardRequest == null || districtCode == null || code == null) {
            return ResponseEntity.status(400).body(
                    DataResponse.<ManageWardDTO>builder()
                            .status("error")
                            .message("Dữ liệu Phường/Xã hoặc mã quận/huyện không hợp lệ")
                            .build()
            );
        }

        ManageWardDTO updatedWard = wardService.updateWard(code, districtCode, wardRequest);

        return ResponseEntity.ok(
                DataResponse.<ManageWardDTO>builder()
                        .status("success")
                        .message("Cập nhật Phường/Xã thành công")
                        .data(updatedWard)
                        .build()
        );
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @DeleteMapping("/admin/ward/delete")
    public ResponseEntity<DataResponse<Void>> deleteWard(@RequestParam String code) {
        try {
            wardService.deleteWardByCode(code);
            return ResponseEntity.ok(
                    DataResponse.<Void>builder()
                            .status("success")
                            .message("Xóa Phường/Xã thành công")
                            .build()
            );
        } catch (Exception e) {
            return ResponseEntity.status(404).body(
                    DataResponse.<Void>builder()
                            .status("error")
                            .message("Không tìm thấy Phường/Xã để xóa")
                            .build()
            );
        }
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/admin/ward/detail")
    public ResponseEntity<DataResponse<ManageWardDTO>> getWardByCode(@RequestParam String code) {
        try {
            ManageWardDTO manageWardDTO = wardService.getWardByCode(code);
            return ResponseEntity.ok(
                    DataResponse.<ManageWardDTO>builder()
                            .status("success")
                            .message("Lấy thông tin Phường/Xã thành công")
                            .data(manageWardDTO)
                            .build()
            );
        } catch (Exception e) {
            return ResponseEntity.status(404).body(
                    DataResponse.<ManageWardDTO>builder()
                            .status("error")
                            .message("Không tìm thấy Phường/Xã")
                            .build()
            );
        }
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/admin/export-ward-csv")
    public void exportWardsToExcel(
            @RequestParam String districtCode,
            @RequestParam(required = false) Optional<String> code,
            @RequestParam(required = false) Optional<String> name,
            @RequestParam(required = false) Optional<String> nameEn,
            @RequestParam(required = false) Optional<String> fullName,
            @RequestParam(required = false) Optional<String> fullNameEn,
            @RequestParam(required = false) Optional<String> codeName,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "code") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir,
            HttpServletResponse response) throws IOException {

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

        Page<ManageWardDTO> wardPage = wardService.getAllWardsWithFilters(
                code, name, nameEn, fullName, fullNameEn, codeName, Optional.of(districtCode), pageable);
        List<ManageWardDTO> wards = wardPage.getContent();

        if (wards.isEmpty()) {
            throw new IOException("Không có Phường/Xã nào để xuất");
        }

        List<String> headers = List.of("Ward Code", "Ward Name", "Name (English)", "Full Name", "Full Name (English)", "Code Name", "District Code");
        List<List<String>> data = wards.stream()
                .map(ward -> List.of(
                        ward.getCode(),
                        ward.getName(),
                        ward.getNameEn(),
                        ward.getFullName(),
                        ward.getFullNameEn(),
                        ward.getCodeName(),
                        districtCode
                ))
                .collect(Collectors.toList());

        String fileName = "Wards_" + excelService.currentDate() + ".csv";

        excelService.generateExcelFile("Wards", headers, data, fileName, response);
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/admin/export-ward-pdf")
    public void exportWardsToPdf(
            @RequestParam String districtCode,
            @RequestParam(required = false) Optional<String> code,
            @RequestParam(required = false) Optional<String> name,
            @RequestParam(required = false) Optional<String> nameEn,
            @RequestParam(required = false) Optional<String> fullName,
            @RequestParam(required = false) Optional<String> fullNameEn,
            @RequestParam(required = false) Optional<String> codeName,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "code") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir,
            HttpServletResponse response) throws DocumentException, IOException {

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

        Page<ManageWardDTO> wardPage = wardService.getAllWardsWithFilters(
                code, name, nameEn, fullName, fullNameEn, codeName, Optional.of(districtCode), pageable);
        List<ManageWardDTO> wards = wardPage.getContent();

        if (wards.isEmpty()) {
            throw new IOException("Không có Phường/Xã nào để xuất");
        }

        String templatePath = "/templates/ward_template.html";
        String dataRows = buildWardDataRows(wards);

        Map<String, String> placeholders = Map.of(
                "{{date}}", pdfService.currentDate(),
                "{{wards}}", dataRows,
                "{{logo_url}}", FilePaths.LOGO_URL
        );

        String fileName = "Wards_" + pdfService.currentDate() + ".pdf";
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

    private String buildWardDataRows(List<ManageWardDTO> wards) {
        StringBuilder dataRows = new StringBuilder();

        for (ManageWardDTO ward : wards) {
            dataRows.append("<tr>")
                    .append("<td>").append(ward.getCode()).append("</td>")
                    .append("<td>").append(ward.getName()).append("</td>")
                    .append("<td>").append(ward.getNameEn()).append("</td>")
                    .append("<td>").append(ward.getFullName()).append("</td>")
                    .append("<td>").append(ward.getFullNameEn()).append("</td>")
                    .append("<td>").append(ward.getCodeName()).append("</td>")
                    .append("<td>").append(ward.getDistrictCode()).append("</td>")
                    .append("</tr>");
        }

        return dataRows.toString();
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/admin/import-ward-csv")
    public ResponseEntity<?> importWardsFromCsv(@RequestParam("file") MultipartFile file) throws IOException {
        List<List<String>> csvData = excelService.importCsv(file);
        wardService.importWards(csvData);

        return ResponseEntity.ok(DataResponse.builder()
                .status("success")
                .message("Import thành công.")
                .build());
    }

}
