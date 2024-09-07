package studentConsulting.controller;

import java.security.Principal;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.payload.dto.RatingDTO;
import studentConsulting.model.payload.request.rating.CreateRatingRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.service.IRatingService;
import studentConsulting.service.IUserService;

@RestController
@RequestMapping("/api/v1/rating")
public class RatingController {
	@Autowired
	private IRatingService ratingService;

	@Autowired
	private IUserService userService;

	@PostMapping("/create")
	public ResponseEntity<DataResponse<RatingDTO>> create(@RequestBody CreateRatingRequest request,
			Principal principal) {
		String username = principal.getName();

		UserInformationEntity user = userService.findByUsername(username)
				.orElseThrow(() -> new RuntimeException("Người dùng không tồn tại"));

		RatingDTO createRating = ratingService.createRating(request, user);
		
		return ResponseEntity.ok(
	            DataResponse.<RatingDTO>builder()
	                .status("success")
	                .message("Mẫu đánh giá đã được tạo thành công.")
	                .data(createRating)
	                .build()
	        );	
	}
	
	@GetMapping("/list/user")
	public ResponseEntity<DataResponse<Page<RatingDTO>>> getRatingsByUser(
	        @RequestParam(required = false) Integer departmentId,
	        @RequestParam(required = false) String consultantName,
	        @RequestParam(defaultValue = "0") int page,
	        @RequestParam(defaultValue = "10") int size,
	        @RequestParam(defaultValue = "submittedAt") String sortBy,
	        @RequestParam(defaultValue = "asc") String sortDir,
	        Principal principal) {

	    // Lấy username của người dùng đang đăng nhập
	    String username = principal.getName();

	    // Tìm thông tin người dùng dựa vào username
	    UserInformationEntity user = userService.findByUsername(username)
	            .orElseThrow(() -> new RuntimeException("Người dùng không tồn tại"));

	    // Tạo đối tượng Pageable cho phân trang và sắp xếp
	    Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));
	    Page<RatingDTO> ratings;

	    // Kiểm tra điều kiện lọc theo departmentId và consultantName
	    if (departmentId != null && consultantName != null) {
	        ratings = ratingService.getRatingsByUserAndDepartmentAndConsultantName(user, departmentId, consultantName, pageable);
	    } else if (departmentId != null) {
	        ratings = ratingService.getRatingsByUserAndDepartment(user, departmentId, pageable);
	    } else if (consultantName != null) {
	        ratings = ratingService.searchRatingsByUserAndConsultantName(user, consultantName, pageable);
	    } else {
	        ratings = ratingService.getAllRatingsByUser(user, pageable);
	    }

	    // Nếu không tìm thấy đánh giá nào, trả về lỗi 404
	    if (ratings.isEmpty()) {
	        return ResponseEntity.status(404).body(
	            DataResponse.<Page<RatingDTO>>builder()
	                .status("error")
	                .message("No ratings found.")
	                .build()
	        );
	    }

	    // Trả về kết quả thành công với dữ liệu đánh giá
	    return ResponseEntity.ok(
	        DataResponse.<Page<RatingDTO>>builder()
	            .status("success")
	            .message("Fetched ratings successfully.")
	            .data(ratings)
	            .build()
	    );
	}

}
