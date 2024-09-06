package studentConsulting.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import studentConsulting.model.payload.dto.CommonQuestionDTO;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.service.ICommonQuestionService;

@RestController
@RequestMapping(value = "/api/v1/common-question")
public class CommonQuestionController {

	@Autowired
	private ICommonQuestionService commonQuestionService;

	@GetMapping("/list")
	public ResponseEntity<DataResponse<List<CommonQuestionDTO>>> getCommons() {
		List<CommonQuestionDTO> commonQuestions = commonQuestionService.getAllCommonQuestions();

		// Kiểm tra nếu không có kết quả
		if (commonQuestions.isEmpty()) {
			DataResponse<List<CommonQuestionDTO>> errorResponse = DataResponse.<List<CommonQuestionDTO>>builder()
					.status("error")
					.message("No common questions found.")
					.build();
			return ResponseEntity.status(404).body(errorResponse); // Trả về mã lỗi 404 nếu danh sách rỗng
		}

		// Trả về kết quả nếu có dữ liệu
		DataResponse<List<CommonQuestionDTO>> response = DataResponse.<List<CommonQuestionDTO>>builder()
				.status("success")
				.message("Fetched all common questions successfully.")
				.data(commonQuestions)
				.build();

		return ResponseEntity.ok(response);
	}

	@GetMapping("/filter-by-department/{departmentId}")
	public ResponseEntity<DataResponse<List<CommonQuestionDTO>>> getCommonQuestionsByDepartment(
			@PathVariable Integer departmentId) {
		List<CommonQuestionDTO> commonQuestions = commonQuestionService.getCommonQuestionsByDepartment(departmentId);

		// Kiểm tra nếu không có kết quả
		if (commonQuestions.isEmpty()) {
			DataResponse<List<CommonQuestionDTO>> errorResponse = DataResponse.<List<CommonQuestionDTO>>builder()
					.status("error")
					.message("No common questions found for department ID: " + departmentId)
					.build();
			return ResponseEntity.status(404).body(errorResponse); // Trả về mã lỗi 404 nếu danh sách rỗng
		}

		// Trả về kết quả nếu có dữ liệu
		DataResponse<List<CommonQuestionDTO>> response = DataResponse.<List<CommonQuestionDTO>>builder()
				.status("success")
				.message("Fetched common questions by department successfully.")
				.data(commonQuestions)
				.build();

		return ResponseEntity.ok(response);
	}
	
	@GetMapping("/search-by-title")
	public ResponseEntity<DataResponse<List<CommonQuestionDTO>>> searchCommonQuestionsByTitle(@RequestParam String title) {
	    List<CommonQuestionDTO> commonQuestions = commonQuestionService.searchCommonQuestionsByTitle(title);

	    // Kiểm tra nếu không tìm thấy kết quả
	    if (commonQuestions.isEmpty()) {
	        DataResponse<List<CommonQuestionDTO>> errorResponse = DataResponse.<List<CommonQuestionDTO>>builder()
	                .status("error")
	                .message("No common questions found with the title: " + title)
	                .build();
	        return ResponseEntity.status(404).body(errorResponse); // Trả về mã lỗi 404
	    }

	    // Trả về kết quả nếu tìm thấy
	    DataResponse<List<CommonQuestionDTO>> successResponse = DataResponse.<List<CommonQuestionDTO>>builder()
	            .status("success")
	            .message("Found common questions by title successfully.")
	            .data(commonQuestions)
	            .build();

	    return ResponseEntity.ok(successResponse);
	}
}
