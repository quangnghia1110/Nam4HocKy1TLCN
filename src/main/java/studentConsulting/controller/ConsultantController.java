package studentConsulting.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
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
	public ResponseEntity<DataResponse<List<ConsultantDTO>>> getConsultants() {
		List<ConsultantDTO> consultants = consultantService.getAllConsultants();

		// Kiểm tra nếu không có kết quả
		if (consultants.isEmpty()) {
			DataResponse<List<ConsultantDTO>> errorResponse = DataResponse.<List<ConsultantDTO>>builder()
					.status("error")
					.message("No consultants found.")
					.build();
			return ResponseEntity.status(404).body(errorResponse); // Trả về mã lỗi 404 nếu danh sách rỗng
		}

		// Trả về kết quả nếu có dữ liệu
		DataResponse<List<ConsultantDTO>> response = DataResponse.<List<ConsultantDTO>>builder()
				.status("success")
				.message("Fetched all consultants successfully.")
				.data(consultants)
				.build();

		return ResponseEntity.ok(response);
	}

	@GetMapping("/filter-by-department/{departmentId}")
	public ResponseEntity<DataResponse<List<ConsultantDTO>>> getConsultantsByDepartment(
			@PathVariable Integer departmentId) {
		List<ConsultantDTO> consultants = consultantService.getConsultantByDepartment(departmentId);

		// Kiểm tra nếu không có kết quả
		if (consultants.isEmpty()) {
			DataResponse<List<ConsultantDTO>> errorResponse = DataResponse.<List<ConsultantDTO>>builder()
					.status("error")
					.message("No consultants found for department ID: " + departmentId)
					.build();
			return ResponseEntity.status(404).body(errorResponse); // Trả về mã lỗi 404 nếu danh sách rỗng
		}

		// Trả về kết quả nếu có dữ liệu
		DataResponse<List<ConsultantDTO>> response = DataResponse.<List<ConsultantDTO>>builder()
				.status("success")
				.message("Fetched consultants by department successfully.")
				.data(consultants)
				.build();

		return ResponseEntity.ok(response);
	}

	@GetMapping("/search-by-name")
	public ResponseEntity<DataResponse<List<ConsultantDTO>>> searchConsultantsByName(@RequestParam String name) {
	    List<ConsultantDTO> consultants = consultantService.searchConsultantsByName(name);

	    // Kiểm tra nếu không tìm thấy kết quả
	    if (consultants.isEmpty()) {
	        DataResponse<List<ConsultantDTO>> errorResponse = DataResponse.<List<ConsultantDTO>>builder()
	                .status("error")
	                .message("No consultants found with the name: " + name)
	                .build();
	        return ResponseEntity.status(404).body(errorResponse); // Trả về mã lỗi 404 nếu không có kết quả
	    }

	    // Trả về kết quả nếu tìm thấy
	    DataResponse<List<ConsultantDTO>> successResponse = DataResponse.<List<ConsultantDTO>>builder()
	            .status("success")
	            .message("Found consultants by name successfully.")
	            .data(consultants)
	            .build();

	    return ResponseEntity.ok(successResponse);
	}
}
