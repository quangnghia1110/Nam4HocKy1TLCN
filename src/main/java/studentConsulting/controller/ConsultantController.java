package studentConsulting.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import studentConsulting.model.payload.dto.ConsultantDTO;
import studentConsulting.model.payload.dto.UserDTO;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.service.IConsultantService;

@RestController
@RequestMapping("${base.url}")
public class ConsultantController {

	@Autowired
	private IConsultantService consultantService;

	@GetMapping("/list-consultant")
    public ResponseEntity<DataResponse<Page<ConsultantDTO>>> getConsultants(
            @RequestParam(required = false) Integer departmentId,
            @RequestParam(required = false) String name,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "firstName") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir) {

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));
        Page<ConsultantDTO> consultants = consultantService.getFilteredConsultants(departmentId, name, pageable);

        if (consultants.isEmpty()) {
            return ResponseEntity.status(404).body(
                    DataResponse.<Page<ConsultantDTO>>builder()
                            .status("error")
                            .message("Không tìm thấy tư vấn viên.")
                            .build()
            );
        }

        return ResponseEntity.ok(
                DataResponse.<Page<ConsultantDTO>>builder()
                        .status("success")
                        .message("Lấy danh sách tư vấn viên thành công.")
                        .data(consultants)
                        .build()
        );
    }

	@GetMapping("/list-consultant-by-department")
	public ResponseEntity<DataResponse<List<UserDTO>>> getConsultantsByDepartment(@RequestParam Integer departmentId) {
	    List<UserDTO> consultants = consultantService.getConsultantsByDepartment(departmentId);
	    if (consultants.isEmpty()) {
	        return ResponseEntity.status(404).body(
	            DataResponse.<List<UserDTO>>builder()
	                .status("error")
	                .message("No consultants found.")
	                .build()
	        );
	    }

	    return ResponseEntity.ok(
	        DataResponse.<List<UserDTO>>builder()
	            .status("success")
	            .message("Danh sách tư vấn viên")
	            .data(consultants)
	            .build()
	    );
	}
}
