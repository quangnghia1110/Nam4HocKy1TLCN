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
import studentConsulting.model.payload.dto.address.ManageDistrictDTO;
import studentConsulting.model.payload.request.address.DistrictRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.service.interfaces.admin.IAdminDistrictService;
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
public class AdminDistrictController {

    @Autowired
    private IAdminDistrictService districtService;

    @Autowired
    private ICommonExcelService excelService;

    @Autowired
    private ICommonPdfService pdfService;

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/admin/district/list")
    public ResponseEntity<DataResponse<Page<ManageDistrictDTO>>> getDistricts(
            @RequestParam String provinceCode,
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

        Page<ManageDistrictDTO> districts = districtService.getAllDistrictsWithFilters(code, name, nameEn, fullName, fullNameEn, codeName, Optional.of(provinceCode), pageable);

        if (districts.isEmpty()) {
            return ResponseEntity.status(404).body(
                    DataResponse.<Page<ManageDistrictDTO>>builder()
                            .status("error")
                            .message("Không tìm thấy quận/huyện phù hợp trong tỉnh")
                            .build()
            );
        }

        return ResponseEntity.ok(
                DataResponse.<Page<ManageDistrictDTO>>builder()
                        .status("success")
                        .message("Lấy danh sách quận/huyện thành công")
                        .data(districts)
                        .build()
        );
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/admin/district/create")
    public ResponseEntity<DataResponse<ManageDistrictDTO>> createDistrict(
            @RequestParam String provinceCode,
            @RequestParam String code,
            @RequestBody DistrictRequest districtRequest) {
        if (districtRequest == null || provinceCode == null || code == null) {
            return ResponseEntity.status(400).body(
                    DataResponse.<ManageDistrictDTO>builder()
                            .status("error")
                            .message("Dữ liệu quận/huyện, mã tỉnh hoặc mã quận/huyện không hợp lệ")
                            .build()
            );
        }

        if (districtService.existsByCode(code)) {
            return ResponseEntity.status(400).body(
                    DataResponse.<ManageDistrictDTO>builder()
                            .status("error")
                            .message("Mã quận/huyện đã tồn tại")
                            .build()
            );
        }

        ManageDistrictDTO savedDistrict = districtService.createDistrict(code, provinceCode, districtRequest);

        return ResponseEntity.ok(
                DataResponse.<ManageDistrictDTO>builder()
                        .status("success")
                        .message("Tạo quận/huyện thành công")
                        .data(savedDistrict)
                        .build()
        );
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @PutMapping("/admin/district/update")
    public ResponseEntity<DataResponse<ManageDistrictDTO>> updateDistrict(
            @RequestParam String code,
            @RequestParam String provinceCode,
            @RequestBody DistrictRequest districtRequest) {
        if (districtRequest == null || provinceCode == null || code == null) {
            return ResponseEntity.status(400).body(
                    DataResponse.<ManageDistrictDTO>builder()
                            .status("error")
                            .message("Dữ liệu quận/huyện, mã tỉnh hoặc mã quận/huyện không hợp lệ")
                            .build()
            );
        }

        ManageDistrictDTO updatedDistrict = districtService.updateDistrict(code, provinceCode, districtRequest);

        return ResponseEntity.ok(
                DataResponse.<ManageDistrictDTO>builder()
                        .status("success")
                        .message("Cập nhật quận/huyện thành công")
                        .data(updatedDistrict)
                        .build()
        );
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @DeleteMapping("/admin/district/delete")
    public ResponseEntity<DataResponse<Void>> deleteDistrict(@RequestParam String code) {
        try {
            districtService.deleteDistrictByCode(code);
            return ResponseEntity.ok(
                    DataResponse.<Void>builder()
                            .status("success")
                            .message("Xóa quận/huyện thành công")
                            .build()
            );
        } catch (Exception e) {
            return ResponseEntity.status(404).body(
                    DataResponse.<Void>builder()
                            .status("error")
                            .message("Không tìm thấy quận/huyện để xóa")
                            .build()
            );
        }
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/admin/district/detail")
    public ResponseEntity<DataResponse<ManageDistrictDTO>> getDistrictByCode(@RequestParam String code) {
        try {
            ManageDistrictDTO manageDistrictDTO = districtService.getDistrictByCode(code);
            return ResponseEntity.ok(
                    DataResponse.<ManageDistrictDTO>builder()
                            .status("success")
                            .message("Lấy thông tin quận/huyện thành công")
                            .data(manageDistrictDTO)
                            .build()
            );
        } catch (Exception e) {
            return ResponseEntity.status(404).body(
                    DataResponse.<ManageDistrictDTO>builder()
                            .status("error")
                            .message("Không tìm thấy quận/huyện")
                            .build()
            );
        }
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/admin/export-district-csv")
    public void exportDistrictsToExcel(
            @RequestParam String provinceCode,
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

        Page<ManageDistrictDTO> districtPage = districtService.getAllDistrictsWithFilters(
                code, name, nameEn, fullName, fullNameEn, codeName, Optional.of(provinceCode), pageable);
        List<ManageDistrictDTO> districts = districtPage.getContent();

        if (districts.isEmpty()) {
            throw new IOException("Không có quận/huyện nào để xuất");
        }

        List<String> headers = List.of("District Code", "District Name", "Name (English)", "Full Name", "Full Name (English)", "Code Name", "Province Code");
        List<List<String>> data = districts.stream()
                .map(district -> List.of(
                        district.getCode(),
                        district.getName(),
                        district.getNameEn(),
                        district.getFullName(),
                        district.getFullNameEn(),
                        district.getCodeName(),
                        provinceCode
                ))
                .collect(Collectors.toList());

        String fileName = "Districts_" + excelService.currentDate() + ".csv";

        excelService.generateExcelFile("Districts", headers, data, fileName, response);
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/admin/export-district-pdf")
    public void exportDistrictsToPdf(
            @RequestParam String provinceCode,
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

        Page<ManageDistrictDTO> districtPage = districtService.getAllDistrictsWithFilters(
                code, name, nameEn, fullName, fullNameEn, codeName, Optional.of(provinceCode), pageable);
        List<ManageDistrictDTO> districts = districtPage.getContent();

        if (districts.isEmpty()) {
            throw new IOException("Không có quận/huyện nào để xuất");
        }

        String templatePath = "/templates/district_template.html";
        String dataRows = buildDistrictDataRows(districts);

        Map<String, String> placeholders = Map.of(
                "{{date}}", pdfService.currentDate(),
                "{{districts}}", dataRows,
                "{{logo_url}}", FilePaths.LOGO_URL
        );

        String fileName = "Districts_" + pdfService.currentDate() + ".pdf";
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

    private String buildDistrictDataRows(List<ManageDistrictDTO> districts) {
        StringBuilder dataRows = new StringBuilder();

        for (ManageDistrictDTO district : districts) {
            dataRows.append("<tr>")
                    .append("<td>").append(district.getCode()).append("</td>")
                    .append("<td>").append(district.getName()).append("</td>")
                    .append("<td>").append(district.getNameEn()).append("</td>")
                    .append("<td>").append(district.getFullName()).append("</td>")
                    .append("<td>").append(district.getFullNameEn()).append("</td>")
                    .append("<td>").append(district.getCodeName()).append("</td>")
                    .append("<td>").append(district.getProvinceCode()).append("</td>")
                    .append("</tr>");
        }

        return dataRows.toString();
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/admin/import-district-csv")
    public ResponseEntity<?> importDistrictsFromCsv(@RequestParam("file") MultipartFile file) throws IOException {
        List<List<String>> csvData = excelService.importCsv(file);
        districtService.importDistricts(csvData);

        return ResponseEntity.ok(DataResponse.builder()
                .status("success")
                .message("Import thành công.")
                .build());
    }
}
