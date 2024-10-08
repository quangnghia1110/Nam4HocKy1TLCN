package studentConsulting.controller.user;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;
import studentConsulting.constant.SecurityConstants;
import studentConsulting.model.entity.user.UserInformationEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.rating.RatingDTO;
import studentConsulting.model.payload.request.rating.CreateRatingRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.user.UserRepository;
import studentConsulting.service.interfaces.common.ICommonConsultantService;
import studentConsulting.service.interfaces.common.ICommonUserService;
import studentConsulting.service.interfaces.user.IUserRatingService;

import java.security.Principal;
import java.time.LocalDate;
import java.util.Optional;

@RestController
@RequestMapping("${base.url}")
public class UserRatingController {
    @Autowired
    private IUserRatingService ratingService;

    @Autowired
    private ICommonUserService userService;
    @Autowired
    private UserRepository userRepository;

    @Autowired
    private ICommonConsultantService consultantService;

    @PreAuthorize(SecurityConstants.PreAuthorize.USER)
    @PostMapping("/user/rating/create")
    public ResponseEntity<DataResponse<RatingDTO>> create(@RequestBody CreateRatingRequest request,
                                                          Principal principal) {
        String email = principal.getName();
        System.out.println("Email: " + email);
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();

        RatingDTO createRating = ratingService.createRating(request, user);

        return ResponseEntity.ok(DataResponse.<RatingDTO>builder().status("success")
                .message("Mẫu đánh giá đã được tạo thành công.").data(createRating).build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.USER)
    @GetMapping("/user/rating/list")
    public ResponseEntity<DataResponse<Page<RatingDTO>>> getRatingsByUser(
            @RequestParam(required = false) Integer departmentId, @RequestParam(required = false) String consultantName,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate,
            @RequestParam(defaultValue = "0") int page, @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "submittedAt") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir, Principal principal) {

        String email = principal.getName();
        System.out.println("Email: " + email);
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        // Gọi tầng service để thực hiện lọc dữ liệu
        Page<RatingDTO> ratings = ratingService.getRatingsByUser(email, departmentId, consultantName, startDate,
                endDate, page, size, sortBy, sortDir);

        return ResponseEntity.ok(DataResponse.<Page<RatingDTO>>builder().status("success")
                .message("Fetched ratings successfully.").data(ratings).build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.USER)
    @GetMapping("/user/rating")
    public ResponseEntity<DataResponse<RatingDTO>> getRatingById(@RequestParam("id") Integer ratingId, Principal principal) {
        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        RatingDTO ratingDTO = ratingService.getRatingById(ratingId, email);
        if (ratingDTO == null) {
            throw new ErrorException("Không tìm thấy đánh giá");
        }

        return ResponseEntity.ok(DataResponse.<RatingDTO>builder()
                .status("success")
                .message("Lấy chi tiết đánh giá thành công.")
                .data(ratingDTO)
                .build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.USER)
    @GetMapping("/list-consultant-rating-by-department")
    public ResponseEntity<DataResponse<RatingDTO>> getRatingByConsultant(@RequestParam Integer consultantId, Principal principal) {
        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();

        RatingDTO rating = ratingService.getRatingByConsultantId(consultantId, user.getId());

        return ResponseEntity.ok(
                DataResponse.<RatingDTO>builder()
                        .status("success")
                        .message("Đánh giá của tư vấn viên")
                        .data(rating)
                        .build()
        );
    }

}
