package studentConsulting.constant.enums;

public enum LikeType {
    POST {
        @Override
        public String toString() {
            return "post";
        }
    },
    COMMENT {
        @Override
        public String toString() {
            return "comment";
        }
    }
}

