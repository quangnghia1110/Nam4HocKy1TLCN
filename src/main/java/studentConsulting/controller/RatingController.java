package studentConsulting.controller;

import java.security.Principal;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.payload.dto.ConsultationScheduleDTO;
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

		RatingDTO createRating = ratingService.createRating(user, request);
		
		return ResponseEntity.ok(
	            DataResponse.<RatingDTO>builder()
	                .status("success")
	                .message("Mẫu đánh giá đã được tạo thành công.")
	                .data(createRating)
	                .build()
	        );
		
	}
}
