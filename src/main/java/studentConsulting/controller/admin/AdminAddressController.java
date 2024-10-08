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
import studentConsulting.model.payload.dto.address.ManageAddressDTO;
import studentConsulting.model.payload.request.address.AddressRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.service.interfaces.admin.IAdminAdressService;
import studentConsulting.service.interfaces.common.ICommonExcelService;
import studentConsulting.service.interfaces.common.ICommonPdfService;

import javax.servlet.http.HttpServletResponse;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

@RestController
@RequestMapping("${base.url}")
public class AdminAddressController {

    @Autowired
    private IAdminAdressService addressService;

    @Autowired
    private ICommonExcelService excelService;

    @Autowired
    private ICommonPdfService pdfService;

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/admin/address/list")
    public ResponseEntity<DataResponse<Page<ManageAddressDTO>>> getAddresses(
            @RequestParam(required = false) Integer id,
            @RequestParam(required = false) String line,
            @RequestParam(required = false) String provinceCode,
            @RequestParam(required = false) String districtCode,
            @RequestParam(required = false) String wardCode,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "line") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir) {

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

        Page<ManageAddressDTO> addresses = addressService.getAllAddressesWithFilters(id, line, provinceCode, districtCode, wardCode, pageable);

        if (addresses.isEmpty()) {
            return ResponseEntity.status(404).body(
                    DataResponse.<Page<ManageAddressDTO>>builder()
                            .status("error")
                            .message("Không tìm thấy địa chỉ phù hợp")
                            .build()
            );
        }
        return ResponseEntity.ok(
                DataResponse.<Page<ManageAddressDTO>>builder()
                        .status("success")
                        .message("Lấy danh sách địa chỉ thành công")
                        .data(addresses)
                        .build()
        );
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/admin/address/create")
    public ResponseEntity<DataResponse<ManageAddressDTO>> createAddress(@RequestBody AddressRequest addressRequest) {
        if (addressRequest == null || addressRequest.getProvinceCode() == null || addressRequest.getDistrictCode() == null || addressRequest.getWardCode() == null) {
            return ResponseEntity.status(400).body(
                    DataResponse.<ManageAddressDTO>builder()
                            .status("error")
                            .message("Dữ liệu địa chỉ không hợp lệ")
                            .build()
            );
        }

        ManageAddressDTO savedAddress = addressService.createAddress(addressRequest);

        return ResponseEntity.ok(
                DataResponse.<ManageAddressDTO>builder()
                        .status("success")
                        .message("Tạo địa chỉ thành công")
                        .data(savedAddress)
                        .build()
        );
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @PutMapping("/admin/address/update")
    public ResponseEntity<DataResponse<ManageAddressDTO>> updateAddress(@RequestParam Integer id, @RequestBody AddressRequest addressRequest) {
        if (addressRequest == null || id == null) {
            return ResponseEntity.status(400).body(
                    DataResponse.<ManageAddressDTO>builder()
                            .status("error")
                            .message("Dữ liệu địa chỉ hoặc mã ID không hợp lệ")
                            .build()
            );
        }

        ManageAddressDTO updatedAddress = addressService.updateAddress(id, addressRequest);

        return ResponseEntity.ok(
                DataResponse.<ManageAddressDTO>builder()
                        .status("success")
                        .message("Cập nhật địa chỉ thành công")
                        .data(updatedAddress)
                        .build()
        );
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @DeleteMapping("/admin/address/delete")
    public ResponseEntity<DataResponse<Void>> deleteAddress(@RequestParam Integer id) {
        try {
            addressService.deleteAddressById(id);
            return ResponseEntity.ok(
                    DataResponse.<Void>builder()
                            .status("success")
                            .message("Xóa địa chỉ thành công")
                            .build()
            );
        } catch (Exception e) {
            return ResponseEntity.status(404).body(
                    DataResponse.<Void>builder()
                            .status("error")
                            .message("Không tìm thấy địa chỉ để xóa")
                            .build()
            );
        }
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/admin/address/detail")
    public ResponseEntity<DataResponse<ManageAddressDTO>> getAddressById(@RequestParam Integer id) {
        try {
            ManageAddressDTO addressDTO = addressService.getAddressById(id);
            return ResponseEntity.ok(
                    DataResponse.<ManageAddressDTO>builder()
                            .status("success")
                            .message("Lấy thông tin địa chỉ thành công")
                            .data(addressDTO)
                            .build()
            );
        } catch (Exception e) {
            return ResponseEntity.status(404).body(
                    DataResponse.<ManageAddressDTO>builder()
                            .status("error")
                            .message("Không tìm thấy địa chỉ")
                            .build()
            );
        }
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/admin/export-address-csv")
    public void exportAddressesToExcel(
            @RequestParam(required = false) Integer id,
            @RequestParam(required = false) String line,
            @RequestParam(required = false) String provinceCode,
            @RequestParam(required = false) String districtCode,
            @RequestParam(required = false) String wardCode,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "line") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir,
            HttpServletResponse response) throws IOException {

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

        Page<ManageAddressDTO> addressPage = addressService.getAllAddressesWithFilters(
                id, line, provinceCode, districtCode, wardCode, pageable);
        List<ManageAddressDTO> addresses = addressPage.getContent();

        if (addresses.isEmpty()) {
            throw new IOException("Không có địa chỉ nào để xuất");
        }

        List<String> headers = List.of("Address ID", "Line", "Province Code", "District Code", "Ward Code");
        List<List<String>> data = addresses.stream()
                .map(address -> List.of(
                        address.getId() != null ? address.getId().toString() : "N/A",
                        address.getLine() != null ? address.getLine() : "N/A",
                        address.getProvinceCode() != null ? address.getProvinceCode() : "N/A",
                        address.getDistrictCode() != null ? address.getDistrictCode() : "N/A",
                        address.getWardCode() != null ? address.getWardCode() : "N/A"
                ))
                .collect(Collectors.toList());


        String fileName = "Addresses_" + excelService.currentDate() + ".csv";

        excelService.generateExcelFile("Addresses", headers, data, fileName, response);
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/admin/export-address-pdf")
    public void exportAddressesToPdf(
            @RequestParam(required = false) Integer id,
            @RequestParam(required = false) String line,
            @RequestParam(required = false) String provinceCode,
            @RequestParam(required = false) String districtCode,
            @RequestParam(required = false) String wardCode,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "line") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir,
            HttpServletResponse response) throws DocumentException, IOException {

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

        Page<ManageAddressDTO> addressPage = addressService.getAllAddressesWithFilters(
                id, line, provinceCode, districtCode, wardCode, pageable);
        List<ManageAddressDTO> addresses = addressPage.getContent();

        if (addresses.isEmpty()) {
            throw new IOException("Không có địa chỉ nào để xuất");
        }

        String templatePath = "/templates/address_template.html";
        String dataRows = buildAddressDataRows(addresses);

        Map<String, String> placeholders = Map.of(
                "{{date}}", pdfService.currentDate(),
                "{{addresses}}", dataRows,
                "{{logo_url}}", FilePaths.LOGO_URL
        );

        String fileName = "Addresses_" + pdfService.currentDate() + ".pdf";
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

    private String buildAddressDataRows(List<ManageAddressDTO> addresses) {
        StringBuilder dataRows = new StringBuilder();

        for (ManageAddressDTO address : addresses) {
            dataRows.append("<tr>")
                    .append("<td>").append(address.getId()).append("</td>")
                    .append("<td>").append(address.getLine()).append("</td>")
                    .append("<td>").append(address.getProvinceCode()).append("</td>")
                    .append("<td>").append(address.getDistrictCode()).append("</td>")
                    .append("<td>").append(address.getWardCode()).append("</td>")
                    .append("</tr>");
        }

        return dataRows.toString();
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/admin/import-address-csv")
    public ResponseEntity<?> importAddressesFromCsv(@RequestParam("file") MultipartFile file) throws IOException {
        List<List<String>> csvData = excelService.importCsv(file);
        addressService.importAddresses(csvData);

        return ResponseEntity.ok(DataResponse.builder()
                .status("success")
                .message("Import thành công.")
                .build());
    }

}
