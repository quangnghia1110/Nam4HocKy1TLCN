package studentConsulting.service.implement.actor;

import com.cloudinary.Cloudinary;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import studentConsulting.constant.SecurityConstants;
import studentConsulting.model.entity.PostEntity;
import studentConsulting.model.entity.UserInformationEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.actor.PostDTO;
import studentConsulting.model.payload.mapper.admin.PostMapper;
import studentConsulting.model.payload.request.CreatePostRequest;
import studentConsulting.model.payload.request.UpdatePostRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.actor.CommentRepository;
import studentConsulting.repository.actor.PostRepository;
import studentConsulting.repository.admin.UserRepository;
import studentConsulting.service.implement.common.FileStorageServiceImpl;
import studentConsulting.service.interfaces.actor.IPostService;
import studentConsulting.specification.actor.PostSpecification;

import java.security.Principal;
import java.time.LocalDate;
import java.util.Optional;

@Service
public class PostServiceImpl implements IPostService {

    @Autowired
    private PostRepository postRepository;

    @Autowired
    private CommentRepository commentRepository;

    @Autowired
    private Cloudinary cloudinary;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private FileStorageServiceImpl fileStorageService;

    @Autowired
    private PostMapper postMapper;

    @Override
    public DataResponse<PostDTO> createPost(CreatePostRequest postRequest, Integer userId) {
        Optional<UserInformationEntity> userOpt = userRepository.findById(userId);
        if (userOpt.isEmpty()) {
            throw new ErrorException("Người dùng không tồn tại.");
        }

        UserInformationEntity user = userOpt.get();
        String fileName = postRequest.getFile() != null && !postRequest.getFile().isEmpty()
                ? fileStorageService.saveFile(postRequest.getFile())
                : null;

        PostEntity post = PostEntity.builder()
                .title(postRequest.getTitle())
                .content(postRequest.getContent())
                .isAnonymous(postRequest.isAnonymous())
                .fileName(fileName)
                .user(user)
                .createdAt(LocalDate.now())
                .isApproved(false)
                .views(0)
                .build();

        PostEntity savedPost = postRepository.save(post);

        PostDTO postDTO = postMapper.mapToDTO(savedPost);
        return new DataResponse<>(postDTO);
    }


    @Override
    public DataResponse<PostDTO> getPostById(Integer id, Integer userId) {
        PostEntity post = postRepository.findById(id)
                .orElseThrow(() -> new ErrorException("Không tìm thấy bài viết."));
        UserInformationEntity user = userRepository.findById(userId)
                .orElseThrow(() -> new ErrorException("Người dùng không tồn tại."));

        String userRole = user.getAccount().getRole().getName();
        boolean isAdmin = userRole.equals(SecurityConstants.Role.ADMIN);
        boolean isAdvisorOrConsultant = userRole.equals(SecurityConstants.Role.TRUONGBANTUVAN) || userRole.equals(SecurityConstants.Role.TUVANVIEN);
        Integer postOwnerId = post.getUser().getId();
        boolean isUser = userRole.equals(SecurityConstants.Role.USER);

        if (!isAdmin && (!isAdvisorOrConsultant || !postOwnerId.equals(userId)) && !isUser) {
            throw new ErrorException("Bạn không có quyền xem bài viết này.");
        }

        PostDTO postDTO = postMapper.mapToDTO(post);
        return DataResponse.<PostDTO>builder()
                .status("success")
                .message("Lấy thông tin bài viết thành công")
                .data(postDTO)
                .build();
    }


    @Override
    public DataResponse<PostDTO> updatePost(Integer id, UpdatePostRequest postRequest, Integer userId) {
        PostEntity post = postRepository.findById(id).orElseThrow(() -> new ErrorException("Không tìm thấy bài viết."));
        UserInformationEntity user = userRepository.findById(userId).orElseThrow(() -> new ErrorException("Người dùng không tồn tại."));

        boolean isAdmin = user.getAccount().getRole().getName().equals(SecurityConstants.Role.ADMIN);
        if (!isAdmin && !post.getUser().getId().equals(userId)) {
            throw new ErrorException("Bạn chỉ có thể cập nhật bài viết của chính mình.");
        }

        post.setTitle(postRequest.getTitle());
        post.setContent(postRequest.getContent());
        post.setAnonymous(postRequest.isAnonymous());

        if (postRequest.getFile() != null && !postRequest.getFile().isEmpty()) {
            String fileName = fileStorageService.saveFile(postRequest.getFile());
            post.setFileName(fileName);
        }

        PostEntity updatedPost = postRepository.save(post);
        return DataResponse.<PostDTO>builder()
                .status("success")
                .message("Bài viết đã được cập nhật thành công")
                .data(postMapper.mapToDTO(updatedPost))
                .build();
    }

    @Override
    public DataResponse<String> deletePost(Integer id, Integer userId) {
        PostEntity post = postRepository.findById(id).orElseThrow(() -> new ErrorException("Không tìm thấy bài viết."));
        UserInformationEntity user = userRepository.findById(userId).orElseThrow(() -> new ErrorException("Người dùng không tồn tại."));

        boolean isAdmin = user.getAccount().getRole().getName().equals(SecurityConstants.Role.ADMIN);
        if (!isAdmin && !post.getUser().getId().equals(userId)) {
            throw new ErrorException("Bạn chỉ có thể xóa bài viết của chính mình.");
        }

        postRepository.deleteById(post.getId());
        return DataResponse.<String>builder()
                .status("success")
                .message("Bài viết đã được xóa thành công")
                .build();
    }

    @Override
    public Page<PostDTO> getAllPostsWithFilters(boolean isApproved, Optional<LocalDate> startDate, Optional<LocalDate> endDate, Pageable pageable) {
        Specification<PostEntity> spec = Specification.where(PostSpecification.isApproved(isApproved));

        if (startDate.isPresent() && endDate.isPresent()) {
            spec = spec.and(PostSpecification.hasExactDateRange(startDate.get(), endDate.get()));
        } else if (startDate.isPresent()) {
            spec = spec.and(PostSpecification.hasExactStartDate(startDate.get()));
        } else if (endDate.isPresent()) {
            spec = spec.and(PostSpecification.hasDateBefore(endDate.get()));
        }

        return postRepository.findAll(spec, pageable).map(postMapper::mapToDTO);
    }

    @Override
    public Page<PostDTO> getPostByRole(boolean isApproved, Optional<LocalDate> startDate, Optional<LocalDate> endDate, Pageable pageable, Principal principal) {
        // Lấy email từ Principal
        String email = principal.getName();
        System.out.println("Email: " + email);

        UserInformationEntity user = userRepository.findUserInfoByEmail(email)
                .orElseThrow(() -> new ErrorException("Không tìm thấy người dùng"));
        System.out.println("User ID: " + user.getId());

        String userRole = user.getAccount().getRole().getName();
        System.out.println("User Role: " + userRole);

        Specification<PostEntity> spec = Specification.where(PostSpecification.isApproved(isApproved));
        System.out.println("Initial Specification (isApproved): " + isApproved);

        if (userRole.equals(SecurityConstants.Role.ADMIN)) {
            System.out.println("Role is ADMIN: No additional filters applied.");
        } else if (userRole.equals(SecurityConstants.Role.TRUONGBANTUVAN) || userRole.equals(SecurityConstants.Role.TUVANVIEN)) {
            Integer userId = user.getId();
            spec = spec.and(PostSpecification.hasUserId(userId));
            System.out.println("Specification with userId filter: " + userId);
        } else {
            throw new ErrorException("Bạn không có quyền truy cập vào danh sách bài viết.");
        }

        if (startDate.isPresent() && endDate.isPresent()) {
            spec = spec.and(PostSpecification.hasExactDateRange(startDate.get(), endDate.get()));
            System.out.println("Specification with date range: " + startDate.get() + " to " + endDate.get());
        } else if (startDate.isPresent()) {
            spec = spec.and(PostSpecification.hasExactStartDate(startDate.get()));
            System.out.println("Specification with start date: " + startDate.get());
        } else if (endDate.isPresent()) {
            spec = spec.and(PostSpecification.hasDateBefore(endDate.get()));
            System.out.println("Specification with end date: " + endDate.get());
        }

        try {
            Page<PostEntity> posts = postRepository.findAll(spec, pageable);
            System.out.println("Total Posts Retrieved: " + posts.getTotalElements());

            Page<PostDTO> postDTOs = posts.map(post -> postMapper.mapToDTO(post));
            System.out.println("Total DTOs Mapped: " + postDTOs.getTotalElements());

            return postDTOs;
        } catch (Exception e) {
            e.printStackTrace();
            throw new ErrorException("Đã xảy ra lỗi trong quá trình truy vấn bài viết.");
        }

    }
}
