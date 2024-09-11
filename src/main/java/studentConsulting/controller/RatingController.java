package studentConsulting.controller;

import java.security.Principal;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.RatingDTO;
import studentConsulting.model.payload.request.rating.CreateRatingRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.service.IRatingService;
import studentConsulting.service.IUserService;

@RestController
@RequestMapping("${base.url}")
public class RatingController {
	@Autowired
	private IRatingService ratingService;

	@Autowired
	private IUserService userService;

	@PreAuthorize("hasRole('USER')")
	@PostMapping("/user/rating/create")
	public ResponseEntity<DataResponse<RatingDTO>> create(@RequestBody CreateRatingRequest request,
			Principal principal) {
		String username = principal.getName();

		UserInformationEntity user = userService.findByUsername(username)
				.orElseThrow(() -> new ErrorException("Người dùng không tồn tại"));

		RatingDTO createRating = ratingService.createRating(request, user);

		return ResponseEntity.ok(
	            DataResponse.<RatingDTO>builder()
	                .status("success")
	                .message("Mẫu đánh giá đã được tạo thành công.")
	                .data(createRating)
	                .build()
	        );
	}

	@PreAuthorize("hasRole('USER')")
	@GetMapping("/user/rating/list")
	public ResponseEntity<DataResponse<Page<RatingDTO>>> getRatingsByUser(
	        @RequestParam(required = false) Integer departmentId,
	        @RequestParam(required = false) String consultantName,
	        @RequestParam(defaultValue = "0") int page,
	        @RequestParam(defaultValue = "10") int size,
	        @RequestParam(defaultValue = "submittedAt") String sortBy,
	        @RequestParam(defaultValue = "asc") String sortDir,
	        Principal principal) {

	    String username = principal.getName();

	    // Gọi tầng service để thực hiện lọc dữ liệu
	    Page<RatingDTO> ratings = ratingService.getRatingsByUser(username, departmentId, consultantName, page, size, sortBy, sortDir);

	    return ResponseEntity.ok(
	        DataResponse.<Page<RatingDTO>>builder()
	            .status("success")
	            .message("Fetched ratings successfully.")
	            .data(ratings)
	            .build()
	    );
	}
}

