package studentConsulting.controller;

import java.security.Principal;
import java.util.List;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.PostDTO;
import studentConsulting.model.payload.request.news.CreatePostRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.UserRepository;
import studentConsulting.service.IPostService;

@RestController
@RequestMapping("${base.url}")
public class PostController {

    @Autowired
    private IPostService postService;

    @Autowired
    private UserRepository userRepository;

    @PreAuthorize("hasRole('TUVANVIEN') or hasRole('TRUONGBANTUVAN')")
    @PostMapping("/post")
    public ResponseEntity<DataResponse<PostDTO>> createPost(
            @ModelAttribute CreatePostRequest postRequest,
            Principal principal) {
        
        String username = principal.getName();
        Optional<UserInformationEntity> userOptional = userRepository.findByAccountUsername(username);
        
        if (userOptional.isEmpty()) {
            throw new ErrorException("Không tìm thấy người dùng.");
        }

        UserInformationEntity user = userOptional.get();
        Integer userId = user.getId();

        DataResponse<PostDTO> response = postService.createPost(postRequest, userId);
        return new ResponseEntity<>(response, HttpStatus.CREATED);
    }
    
    @PreAuthorize("hasRole('TUVANVIEN') or hasRole('TRUONGBANTUVAN')")
    @GetMapping("/post/pending")
    public ResponseEntity<DataResponse<List<PostDTO>>> getPendingPosts(Principal principal) {
        String username = principal.getName();
        Optional<UserInformationEntity> userOptional = userRepository.findByAccountUsername(username);

        if (userOptional.isEmpty()) {
            throw new ErrorException("Không tìm thấy người dùng.");
        }

        UserInformationEntity user = userOptional.get();
        String userId = user.getId().toString();

        DataResponse<List<PostDTO>> response = postService.getPendingPostsByUser(userId);
        return new ResponseEntity<>(response, HttpStatus.OK);
    }
    
    @PreAuthorize("hasRole('ADMIN')")
    @PostMapping("/admin/post/approve")
    public ResponseEntity<DataResponse<PostDTO>> approvePost(@RequestParam Long id) {
        DataResponse<PostDTO> response = postService.approvePost(id);
        return ResponseEntity.ok(response);
    }
}

