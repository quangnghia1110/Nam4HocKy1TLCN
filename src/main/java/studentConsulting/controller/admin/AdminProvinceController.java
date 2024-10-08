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
import studentConsulting.model.payload.dto.address.ManageProvinceDTO;
import studentConsulting.model.payload.request.address.ProvinceRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.service.interfaces.admin.IAdminProvinceService;
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
public class AdminProvinceController {

    @Autowired
    private IAdminProvinceService provinceService;

    @Autowired
    private ICommonExcelService excelService;

    @Autowired
    private ICommonPdfService pdfService;

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/admin/province/list")
    public ResponseEntity<DataResponse<Page<ManageProvinceDTO>>> getProvinces(
            @RequestParam(required = false) String code,
            @RequestParam(required = false) String name,
            @RequestParam(required = false) String nameEn,
            @RequestParam(required = false) String fullName,
            @RequestParam(required = false) String fullNameEn,
            @RequestParam(required = false) String codeName,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "code") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir) {

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

        Page<ManageProvinceDTO> provinces = provinceService.getAllProvincesWithFilters(
                Optional.ofNullable(code),
                Optional.ofNullable(name),
                Optional.ofNullable(nameEn),
                Optional.ofNullable(fullName),
                Optional.ofNullable(fullNameEn),
                Optional.ofNullable(codeName),
                pageable
        );

        if (provinces.isEmpty()) {
            return ResponseEntity.status(404).body(
                    DataResponse.<Page<ManageProvinceDTO>>builder()
                            .status("error")
                            .message("Không tìm thấy tỉnh/thành phù hợp")
                            .build()
            );
        }

        return ResponseEntity.ok(
                DataResponse.<Page<ManageProvinceDTO>>builder()
                        .status("success")
                        .message("Lấy danh sách tỉnh/thành phố thành công")
                        .data(provinces)
                        .build()
        );
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/admin/province/create")
    public ResponseEntity<DataResponse<ManageProvinceDTO>> createProvince(
            @RequestParam String code,
            @RequestBody ProvinceRequest provinceRequest) {
        if (provinceRequest == null || code == null || code.trim().isEmpty()) {
            return ResponseEntity.status(400).body(
                    DataResponse.<ManageProvinceDTO>builder()
                            .status("error")
                            .message("Dữ liệu tỉnh/thành không hợp lệ hoặc thiếu mã")
                            .build()
            );
        }

        if (provinceService.existsByCode(code)) {
            return ResponseEntity.status(400).body(
                    DataResponse.<ManageProvinceDTO>builder()
                            .status("error")
                            .message("Mã tỉnh/thành đã tồn tại")
                            .build()
            );
        }

        ManageProvinceDTO savedProvince = provinceService.createProvince(provinceRequest, code);

        return ResponseEntity.ok(
                DataResponse.<ManageProvinceDTO>builder()
                        .status("success")
                        .message("Tạo tỉnh/thành phố thành công")
                        .data(savedProvince)
                        .build()
        );
    }


    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @PutMapping("/admin/province/update")
    public ResponseEntity<DataResponse<ManageProvinceDTO>> updateProvince(@RequestParam String code, @RequestBody ProvinceRequest provinceRequest) {
        if (provinceRequest == null) {
            return ResponseEntity.status(400).body(
                    DataResponse.<ManageProvinceDTO>builder()
                            .status("error")
                            .message("Dữ liệu tỉnh/thành không hợp lệ")
                            .build()
            );
        }

        ManageProvinceDTO updatedProvince = provinceService.updateProvince(code, provinceRequest);

        return ResponseEntity.ok(
                DataResponse.<ManageProvinceDTO>builder()
                        .status("success")
                        .message("Cập nhật tỉnh/thành phố thành công")
                        .data(updatedProvince)
                        .build()
        );
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @DeleteMapping("/admin/province/delete")
    public ResponseEntity<DataResponse<Void>> deleteProvince(@RequestParam String code) {
        try {
            provinceService.deleteProvinceByCode(code);
            return ResponseEntity.ok(
                    DataResponse.<Void>builder()
                            .status("success")
                            .message("Xóa tỉnh/thành phố thành công")
                            .build()
            );
        } catch (Exception e) {
            return ResponseEntity.status(404).body(
                    DataResponse.<Void>builder()
                            .status("error")
                            .message("Không tìm thấy tỉnh/thành phố để xóa")
                            .build()
            );
        }
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/admin/province/detail")
    public ResponseEntity<DataResponse<ManageProvinceDTO>> getProvinceByCode(@RequestParam String code) {
        try {
            ManageProvinceDTO ManageProvinceDTO = provinceService.getProvinceByCode(code);
            return ResponseEntity.ok(
                    DataResponse.<ManageProvinceDTO>builder()
                            .status("success")
                            .message("Lấy thông tin tỉnh/thành phố thành công")
                            .data(ManageProvinceDTO)
                            .build()
            );
        } catch (Exception e) {
            return ResponseEntity.status(404).body(
                    DataResponse.<ManageProvinceDTO>builder()
                            .status("error")
                            .message("Không tìm thấy tỉnh/thành phố")
                            .build()
            );
        }
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/admin/export-province-csv")
    public void exportProvincesToExcel(
            @RequestParam(required = false) String code,
            @RequestParam(required = false) String name,
            @RequestParam(required = false) String nameEn,
            @RequestParam(required = false) String fullName,
            @RequestParam(required = false) String fullNameEn,
            @RequestParam(required = false) String codeName,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "code") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir,
            HttpServletResponse response) throws IOException {

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

        Page<ManageProvinceDTO> provincePage = provinceService.getAllProvincesWithFilters(
                Optional.ofNullable(code), Optional.ofNullable(name), Optional.ofNullable(nameEn),
                Optional.ofNullable(fullName), Optional.ofNullable(fullNameEn), Optional.ofNullable(codeName), pageable);
        List<ManageProvinceDTO> provinces = provincePage.getContent();

        if (provinces.isEmpty()) {
            throw new IOException("Không có tỉnh/thành nào để xuất");
        }

        List<String> headers = List.of("Province Code", "Province Name", "Name (English)", "Full Name", "Full Name (English)", "Code Name");
        List<List<String>> data = provinces.stream()
                .map(province -> List.of(
                        province.getCode(),
                        province.getName(),
                        province.getNameEn(),
                        province.getFullName(),
                        province.getFullNameEn(),
                        province.getCodeName()
                ))
                .collect(Collectors.toList());

        String fileName = "Provinces_" + excelService.currentDate() + ".csv";

        excelService.generateExcelFile("Provinces", headers, data, fileName, response);
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/admin/export-province-pdf")
    public void exportProvincesToPdf(
            @RequestParam(required = false) String code,
            @RequestParam(required = false) String name,
            @RequestParam(required = false) String nameEn,
            @RequestParam(required = false) String fullName,
            @RequestParam(required = false) String fullNameEn,
            @RequestParam(required = false) String codeName,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "code") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir,
            HttpServletResponse response) throws DocumentException, IOException {

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

        Page<ManageProvinceDTO> provincePage = provinceService.getAllProvincesWithFilters(
                Optional.ofNullable(code), Optional.ofNullable(name), Optional.ofNullable(nameEn),
                Optional.ofNullable(fullName), Optional.ofNullable(fullNameEn), Optional.ofNullable(codeName), pageable);
        List<ManageProvinceDTO> provinces = provincePage.getContent();

        if (provinces.isEmpty()) {
            throw new IOException("Không có tỉnh/thành nào để xuất");
        }

        String templatePath = "/templates/province_template.html";
        String dataRows = buildProvinceDataRows(provinces);

        Map<String, String> placeholders = Map.of(
                "{{date}}", pdfService.currentDate(),
                "{{provinces}}", dataRows,
                "{{logo_url}}", FilePaths.LOGO_URL
        );

        String fileName = "Provinces_" + pdfService.currentDate() + ".pdf";
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

    private String buildProvinceDataRows(List<ManageProvinceDTO> provinces) {
        StringBuilder dataRows = new StringBuilder();

        for (ManageProvinceDTO province : provinces) {
            dataRows.append("<tr>")
                    .append("<td>").append(province.getCode()).append("</td>")
                    .append("<td>").append(province.getName()).append("</td>")
                    .append("<td>").append(province.getNameEn()).append("</td>")
                    .append("<td>").append(province.getFullName()).append("</td>")
                    .append("<td>").append(province.getFullNameEn()).append("</td>")
                    .append("<td>").append(province.getCodeName()).append("</td>")
                    .append("</tr>");
        }

        return dataRows.toString();
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/admin/import-province-csv")
    public ResponseEntity<?> importProvincesFromCsv(@RequestParam("file") MultipartFile file) throws IOException {
        List<List<String>> csvData = excelService.importCsv(file);
        provinceService.importProvinces(csvData);

        return ResponseEntity.ok(DataResponse.builder()
                .status("success")
                .message("Import thành công.")
                .build());
    }

}
