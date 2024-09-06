package studentConsulting.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import studentConsulting.model.payload.dto.ConsultantDTO;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.service.IConsultantService;

@RestController
@RequestMapping(value = "/api/v1/consultant")
public class ConsultantController {

	@Autowired
	private IConsultantService consultantService;

	@GetMapping("/list")
	public ResponseEntity<DataResponse<Page<ConsultantDTO>>> getConsultants(@RequestParam(defaultValue = "0") int page,
			@RequestParam(defaultValue = "10") int size, @RequestParam(defaultValue = "firstName") String sortBy,
			@RequestParam(defaultValue = "asc") String sortDir) {

		// Tạo Pageable để phân trang và sắp xếp
		Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

		// Gọi service để lấy danh sách tư vấn viên với phân trang
		Page<ConsultantDTO> consultants = consultantService.getAllConsultants(pageable);

		// Kiểm tra nếu không có kết quả
		if (consultants.isEmpty()) {
			DataResponse<Page<ConsultantDTO>> errorResponse = DataResponse.<Page<ConsultantDTO>>builder()
					.status("error").message("No consultants found.").build();
			return ResponseEntity.status(404).body(errorResponse); // Trả về mã lỗi 404 nếu danh sách rỗng
		}

		// Trả về kết quả nếu có dữ liệu
		DataResponse<Page<ConsultantDTO>> response = DataResponse.<Page<ConsultantDTO>>builder().status("success")
				.message("Fetched all consultants successfully.").data(consultants).build();

		return ResponseEntity.ok(response);
	}

	@GetMapping("/filter-by-department/{departmentId}")
	public ResponseEntity<DataResponse<Page<ConsultantDTO>>> getConsultantsByDepartment(
			@PathVariable Integer departmentId, @RequestParam(defaultValue = "0") int page,
			@RequestParam(defaultValue = "10") int size, @RequestParam(defaultValue = "firstName") String sortBy,
			@RequestParam(defaultValue = "asc") String sortDir) {

		Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

		Page<ConsultantDTO> consultants = consultantService.getConsultantByDepartment(departmentId, pageable);

		if (consultants.isEmpty()) {
			DataResponse<Page<ConsultantDTO>> errorResponse = DataResponse.<Page<ConsultantDTO>>builder()
					.status("error").message("No consultants found for department ID: " + departmentId).build();
			return ResponseEntity.status(404).body(errorResponse);
		}

		DataResponse<Page<ConsultantDTO>> response = DataResponse.<Page<ConsultantDTO>>builder().status("success")
				.message("Fetched consultants by department successfully.").data(consultants).build();

		return ResponseEntity.ok(response);
	}

	@GetMapping("/search-by-name")
	public ResponseEntity<DataResponse<Page<ConsultantDTO>>> searchConsultantsByName(@RequestParam String name,
			@RequestParam(defaultValue = "0") int page, @RequestParam(defaultValue = "10") int size,
			@RequestParam(defaultValue = "firstName") String sortBy,
			@RequestParam(defaultValue = "asc") String sortDir) {

		Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

		Page<ConsultantDTO> consultants = consultantService.searchConsultantsByName(name, pageable);

		if (consultants.isEmpty()) {
			DataResponse<Page<ConsultantDTO>> errorResponse = DataResponse.<Page<ConsultantDTO>>builder()
					.status("error").message("No consultants found with the name: " + name).build();
			return ResponseEntity.status(404).body(errorResponse);
		}

		DataResponse<Page<ConsultantDTO>> response = DataResponse.<Page<ConsultantDTO>>builder().status("success")
				.message("Found consultants by name successfully.").data(consultants).build();

		return ResponseEntity.ok(response);
	}

}
